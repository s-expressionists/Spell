;;;; This file contains the implementation of the family of functions
;;;; that find dictionary entries which are similar to a given string.

(cl:in-package #:spell)

;;; The generic function `node-map-similar' accepts a query string, a
;;; remaining suffix length within that string and a threshold for the
;;; number of remaining edit operations.  `node-map-similar' traverses
;;; the trie, visiting nodes that can be reached by applying a number
;;; of insert, delete and change edits to the query string that is
;;; below the threshold.  Recursive calls to `node-map-similar'
;;; advance the suffix index and possibly decrease the threshold until
;;; either a leaf is reached, the suffix is empty or the threshold
;;; drops below 0.  The first two cases may correspond to having
;;; located entries that are "similar enough" to the query string with
;;; respect to the initial threshold.
;;;
;;; The basic traversal that is performed by `node-map-similar' is
;;; similar to that performed by `map-node-entries'.  The only (big)
;;; difference is the inclusion of similar children and entries.

;;; This method handles interior nodes by considering each child and
;;; its key which is either a character or a string.  In any case, if
;;; possible, the traversal continues in the respective child with
;;; SUFFIX, THRESHOLD and CHARACTERS adjusted to account for the
;;; matching (possible via "editing" STRING) of the key to a
;;; sub-sequence of STRING.
(defmethod node-map-similar ((function   function)
                             (string     string)
                             (suffix     integer)
                             (node       interior-mixin)
                             (threshold  integer)
                             (characters t))
  (declare (type simple-string string)
           (type a:array-index suffix threshold)
           (type list          characters))
  ;; First look at the keys of all children and determine which
  ;; children can be traversed within the remaining THRESHOLD.
  (let ((string-length (length string)))
    (labels ((visit-child (child suffix threshold characters)
               (funcall function child suffix threshold characters)
               nil) ; return predictable value(s)
             (consider-child/char (key child suffix threshold)
               ;; The key is a single character try the three edit
               ;; operations if THRESHOLD permits.
               (declare (type a:array-index suffix threshold))
               (when (plusp threshold)
                 ;; "insert" edit
                 (let ((characters (list* key characters)))
                   (visit-child child suffix (1- threshold) characters))
                 ;; "delete" edit
                 (when (plusp suffix)
                   (consider-child/char key child (1- suffix) (1- threshold))))
               ;; Match or "change" edit
               (cond ((= suffix 0))
                     ((let ((offset (- string-length suffix)))
                        (char= key (aref string offset)))
                      (let ((characters (list* key characters)))
                        (visit-child child (1- suffix) threshold characters)))
                     ((plusp threshold)
                      (let ((characters (list* key characters)))
                        (visit-child child (1- suffix) (1- threshold) characters)))))
             (consider-child/string (key child suffix threshold)
               (let ((key-length (length key)))
                 ;; The key is a string. Try the three edit operations
                 ;; for each character where THRESHOLD permits.
                 (declare (type simple-string key))
                 (labels
                     ((rec (index suffix threshold characters)
                        (declare (type a:array-index suffix threshold))
                        (cond ((= index key-length)
                               (visit-child child suffix threshold characters))
                              ((let ((offset (- string-length suffix)))
                                 (and (plusp suffix)
                                      (char= (aref key index)
                                             (aref string offset))))
                               (let ((characters (list* (aref key index)
                                                        characters)))
                                 (rec (1+ index) (1- suffix) threshold characters)))
                              ((plusp threshold)
                               ;; "insert" character into query string
                               (let ((characters (list* (aref key index)
                                                        characters)))
                                 (rec (1+ index) suffix (1- threshold) characters))
                               (when (plusp suffix)
                                 ;; "delete" character from query string
                                 (rec index (1- suffix) (1- threshold) characters)
                                 ;; match or "change" edit
                                 (let ((characters (list* (aref key index)
                                                          characters)))
                                   (rec (1+ index) (1- suffix) (1- threshold) characters)))))))
                   (declare (dynamic-extent #'rec))
                   (rec 0 suffix threshold characters))))
             (consider-child (key child)
               (etypecase key
                 (character
                  (consider-child/char key child suffix threshold))
                 (string
                  (consider-child/string key child suffix threshold)))
               ;; `map-children' can write back modifications, so
               ;; avoid returning anything that would be written back.
               nil))
      (declare (dynamic-extent #'visit-child
                               #'consider-child/char
                               #'consider-child/string
                               #'consider-child))
      (map-children #'consider-child node (%children node)))))

;;;; Dictionary-level methods

(defmethod %map-similar ((function   function)
                         (string     string)
                         (dictionary dictionary)
                         (threshold  integer))
  (check-type threshold a:array-index)
  ;; We keep discovered but not yet visited nodes in worklists that
  ;; group the queued nodes by "remaining threshold" which is roughly
  ;; (- THRESHOLD <edit-distance from STRING>) that the node has
  ;; accrued so far.  Since we want to visit nodes with small
  ;; edit-distances first, we dequeue from the bucket with the highest
  ;; remaining threshold.
  (let ((leaf-worklist (make-array (1+ threshold) :initial-element '()))
        (node-worklist (make-array (1+ threshold) :initial-element '())))
    (labels ((enqueue (node suffix remaining-threshold characters)
               (declare (type a:array-index suffix))
               ;; Push NODE into the buckets for REMAINING-THRESHOLD
               ;; in NODE-WORKLIST and LEAF-WORKLIST as appropriate.
               (let ((base (cons node characters))) ; shared list tail
                 ;; If NODE is a leaf and the entire STRING has been
                 ;; matched (modulo edits and possibly dropping a
                 ;; suffix), enqueue NODE for `visit-leaf'.
                 (when (and (<= suffix remaining-threshold) (leafp node))
                   (push base (aref leaf-worklist (- remaining-threshold suffix))))
                 ;; If NODE has children, queue NODE for recursive
                 ;; traversal.
                 (when (interiorp node)
                   (push (cons suffix base)
                         (aref node-worklist remaining-threshold)))))
             (visit-leaf (element remaining-threshold)
               (let ((node       (car element))
                     (characters (cdr element)))
                 (declare (type list characters))
                 ;; STRING has been matched (modulo edits), report all
                 ;; entries in NODE (which may be a proper node or
                 ;; some compact representation) to FUNCTION.
                 (flet ((visit-entry (entry)
                          (let ((spelling (nreverse (coerce characters 'string)))
                                (distance (- threshold remaining-threshold)))
                            (funcall function spelling entry distance))))
                   (declare (dynamic-extent #'visit-entry))
                   (typecase node
                     ((or integer cons)
                      (visit-entry node))
                     (vector
                      (map nil #'visit-entry node))
                     (t
                      (map-leaf-entries #'visit-entry node (%entries node)))))))
             (visit-node (element remaining-threshold)
               (destructuring-bind (suffix node . characters) element
                 ;; Enqueue children of NODE that have a chance to
                 ;; match within REMAINING-THRESHOLD into the
                 ;; appropriate bucket of NODE-WORKLIST.
                 (node-map-similar
                  #'enqueue string suffix node remaining-threshold characters))))
      (declare (dynamic-extent #'enqueue #'visit-leaf #'visit-node))
      ;; Seed NODE-WORKLIST (and possibly LEAF-WORKLIST) with the root
      ;; node.
      (enqueue (contents dictionary) (length string) threshold '())
      ;; Repeatedly grab entries with the smallest edit-distance (so
      ;; far) from either worklist.
      (loop :while (loop :for i :from threshold :downto 0
                         :do (a:when-let ((cluster (aref leaf-worklist i)))
                               (setf (aref leaf-worklist i) (rest cluster))
                               (visit-leaf (first cluster) i)
                               (return t))
                             (a:when-let ((cluster (aref node-worklist i)))
                               (setf (aref node-worklist i) (rest cluster))
                               (visit-node (first cluster) i)
                               (return t))
                         :finally (return nil)))))
  nil)

(defmethod map-similar ((function   t)
                        (string     string)
                        (dictionary dictionary)
                        (threshold  integer)
                        &key (group-by :entry))
  ;; We call `%map-similar' for the basic traversal and add two
  ;; aspects: 1) depending on GROUP-BY, expand compact entries to
  ;; `word' instances 2) detect duplicate either spellings or
  ;; spelling-word pairs and do not report duplicates to FUNCTION.
  (check-type threshold a:array-index)
  (check-type group-by  (member :spelling :entry))
  (let ((function (a:ensure-function function))
        (string   (coerce string 'simple-string))
        (seen     (make-hash-table :test #'equal)))
    (cl:case group-by
      (:spelling
       (flet ((result (spelling entry distance)
                (declare (type a:array-index distance)
                         (ignore entry))
                (let ((existing (gethash spelling seen)))
                  (declare (type (or null a:array-index) existing))
                  (when (null existing)
                    (setf (gethash spelling seen) distance)
                    (funcall function spelling distance)))))
         (declare (dynamic-extent #'result))
         (%map-similar #'result string dictionary threshold)))
      (:entry
       (flet ((result (spelling entry distance)
                (declare (type a:array-index distance))
                (let* ((key      (cons spelling entry))
                       (existing (gethash key seen)))
                  (declare (type (or null a:array-index) existing))
                  (when (null existing)
                    (setf (gethash key seen) distance)
                    (let ((word (expand-entry entry spelling)))
                      (funcall function spelling word distance))))))
         (declare (dynamic-extent #'result))
         (%map-similar #'result string dictionary threshold))))))

(defmethod map-corrections ((function   function)
                            (string     string)
                            (dictionary dictionary)
                            (threshold  integer)
                            &key (variants (lambda (continuation string)
                                             (funcall continuation string)))
                                 (group-by :entry))
  (check-type threshold a:array-index)
  (check-type group-by  (member :spelling :entry))
  (let ((variants (a:ensure-function variants)))
    (labels ((report/spelling (spelling distance)
               (funcall function spelling distance))
             (report/entry (spelling node distance)
               (funcall function spelling node distance))
             (try (variant)
               (let ((collector (cl:case group-by
                                  (:spelling #'report/spelling)
                                  (:entry    #'report/entry))))
                 (map-similar collector variant dictionary threshold
                              :group-by group-by))))
      (declare (dynamic-extent #'report/spelling #'report/entry #'try))
      ;; TODO: this is wrong since results will not be reported in
      ;; order of increasing edit-distance but primarily in order of
      ;; variants tried.
      (funcall variants #'try string))))

(defmethod corrections ((string     string)
                        (dictionary dictionary)
                        (threshold  integer)
                        &key (group-by :entry)
                             (variants nil    variants-supplied-p)
                             count)
  (check-type count (or null a:array-index))
  (let ((results   '())
        (correct?  nil)
        (remaining count))
    ;; The results arrive in increasing-edit-distance order.  We
    ;; expect a small-ish number of results so we use a list.
    (block nil
      (macrolet ((handler ((&rest parameters) value-form)
                   `(lambda (,@parameters distance)
                      (when (zerop distance)
                        (setf correct? t))
                      (cond ((null remaining))
                            ((eql remaining 0) (return))
                            (t                 (decf remaining)))
                      (push ,value-form results))))
        (apply #'map-corrections
               (ecase group-by
                 (:spelling (handler (spelling)      spelling))
                 (:entry    (handler (spelling word) (cons word spelling))))
               string dictionary threshold :group-by group-by
               (when variants-supplied-p (list :variants variants)))))
    ;; Reverse so that the results with the smallest edit-distance are
    ;; at the beginning of the result list.
    (values (nreverse results) correct?)))
