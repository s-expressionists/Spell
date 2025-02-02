(cl:in-package #:spell)

;;; Dictionary

(defclass dictionary (utilities.print-items:print-items-mixin)
  ((%contents    :accessor contents
                 :initform (make-instance 'raw-node))
   (%entry-count :accessor entry-count
                 :initform 0)))

(defmethod make-load-form ((object dictionary) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod utilities.print-items:print-items append ((object dictionary))
  `((:entry-count "~:D entr~:@P" ,(entry-count object))))

(defmethod map-entries ((function t) (dictionary dictionary))
  (let ((function   (a:ensure-function function))
        ;; Since trie nodes do not contain the actual spellings of the
        ;; respective associated words, we keep track of the path to
        ;; the currently visited node in this array by pushing and
        ;; popping characters.
        (characters (make-array 20 :element-type 'character
                                   :adjustable   t
                                   :fill-pointer 0)))
    (flet ((visit (entry characters)
             (let* (;; We copy CHARACTERS since we don't want the base
                    ;; of WORD to change retroactively.
                    (spelling (copy-seq characters))
                    (word     (expand-entry entry spelling)))
               (funcall function spelling word))))
      (declare (dynamic-extent #'visit))
      (map-node-entries #'visit (contents dictionary) characters))))

(defmethod lookup ((string string) (dictionary dictionary))
  (let ((length (length string))
        (result '()))
    (node-lookup (lambda (word)
                   (push word result))
                 string length (contents dictionary))
    result))

(defmethod %map-similar ((function   function) ; TODO: move some other file
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
                   (push base (aref leaf-worklist remaining-threshold)))
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
                        &key (result :entry))
  ;; We call `%map-similar' for the basic traversal and add two
  ;; aspects: 1) depending on RESULT, expand compact entries to `word'
  ;; instances 2) detect duplicate either spellings or spelling-word
  ;; pairs and do not report duplicates to FUNCTION.
  (declare (type a:array-index threshold))
  (check-type threshold a:array-index)  ; TODO:
  (check-type result    (member :spelling :entry))
  (let ((function (a:ensure-function function))
        (seen     (make-hash-table :test #'equal)))
    (cl:case result
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

(defmethod insert ((object t) (string string) (dictionary dictionary))
  (let ((root (contents dictionary)))
    (when (typep root 'compact-node)
      (error "~@<Cannot insert into compacted dictionary.~@:>"))
    (node-insert object string (length string) root)))

(defmethod compact ((dictionary dictionary))
  (setf (contents dictionary) (with-string-interning ()
                                (compact-node (contents dictionary) 0)))
  dictionary)

(defmethod share-structure ((dictionary dictionary) &rest args &key depth-limit)
  (declare (ignore depth-limit))
  (setf (contents dictionary) (apply #'share-structure (contents dictionary)
                                     args))
  dictionary)

;;; Dictionary loading

(defmethod load-dictionary ((source stream)
                            &key (into (make-instance 'dictionary)))
  (let ((count 0))
    (with-string-interning ()
      (map-dictionary-file-entries
       (lambda (spelling type base &rest initargs)
         (let* ((spelling (intern-string spelling))
                (base     (intern-string base))
                (word     (apply #'make-word spelling type base initargs)))
           (insert word spelling into))
         (incf count))
       source))
    (incf (entry-count into) count)
    into))

(defmethod load-dictionary ((source pathname) &rest args &key into)
  (declare (ignore into))
  (with-open-file (stream source)
    (apply #'load-dictionary stream args)))

(defmethod load-dictionary ((source string) &rest args &key into)
  (declare (ignore into))
  (apply #'load-dictionary (pathname source) args))

(defmethod load-dictionary ((source sequence)
                            &key (into (make-instance 'dictionary)))
  (mapc (lambda (source)
          (with-simple-restart (skip "Skip source ~A" source)
            (apply #'load-dictionary source :into into)))
        source))
