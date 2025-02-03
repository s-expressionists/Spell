(cl:in-package #:spell)

;;; This file contains the implementation of `node-map-similar' which
;;; accepts a query string, a remaining suffix length within that
;;; string and a threshold for the number of remaining edit
;;; operations.  `node-map-similar' traverses the trie, visiting nodes
;;; that can be reached by applying a number of insert, delete and
;;; change edits to the query string that is below the threshold.
;;; Recursive calls to `node-map-similar' advance the suffix index and
;;; possibly decrease the threshold until either a leaf is reached,
;;; the suffix is empty or the threshold drops below 0.  The first two
;;; cases may correspond to having located entries that are "similar
;;; enough" to the query string with respect to the initial threshold.
;;;
;;; The basic traversal that is performed by `node-map-similar' is
;;; similar to that performed by `map-node-entries'.  The only (big
;;; difference) is the inclusion of similar children and entries.

;;; The following three methods handle leaf nodes.  The entries that
;;; are associated with those leaf nodes are "similar enough" to the
;;; query STRING with respect to THRESHOLD if we have enough THRESHOLD
;;; left to "delete" the remaining SUFFIX of STRING.
;;;
;;; For example, if we reach a leaf node that is associated with an
;;; entry for "house" and STRING is "houses", then SUFFIX must be 1.
;;; If THRESHOLD is 1 or more, we can still consider the entry as
;;; similar since we need one "delete" operation to ignore the "s"
;;; suffix in STRING.
;;;
;;; We assume that THRESHOLD is not negative since the traversal would
;;; have stopped already if THRESHOLD had dropped below 0.
;;;
;;; CHARACTERS contains the, possibly edited, version of STRING that
;;; allowed the traversal to reach NODE.  When that string must be
;;; preserved, FUNCTION is responsible for making a copy of CHARACTERS
;;; since CHARACTERS will be modified when the traversal continues.

(macrolet
    ((define (node-class &body body)
       `(defmethod node-map-similar ((function   function)
                                     (string     sequence)
                                     (suffix     integer)
                                     (node       ,node-class)
                                     (threshold  integer)
                                     (characters vector))
          (assert (not (minusp threshold)))
          ;; If THRESHOLD permits, use "delete" edits to get rid of
          ;; the leftover suffix of STRING.
          (when (<= suffix threshold)
            (let ((new-threshold (- threshold suffix)))
              ,@body)))))
  (define leaf-mixin
    (map-leaf-entries (lambda (entry)
                        (funcall function entry characters new-threshold))
                      node (%entries node)))
  ;; The next two methods apply to the three kinds of compressed leaf
  ;; nodes, namely vectors of "info" integers, conses of an "info"
  ;; integer and a base string and just "info" integers.
  (define t ; integer and cons
    (funcall function node characters new-threshold))
  (define vector
    (map nil (lambda (entry)
               (funcall function entry characters new-threshold))
         node)))

;;; The next method handles interior nodes by considering each child
;;; and its key which is either a character or a string.  In any case,
;;; if possible, the traversal continues in the respective child with
;;; SUFFIX, THRESHOLD and CHARACTERS adjust to account for the
;;; matching of the key to a sub-sequence of STRING.

(defmacro with-character ((character characters) &body body)
  (a:once-only (characters)
    `(progn
       (vector-push-extend ,character ,characters)
       (prog1
           (progn ,@body))
       (vector-pop ,characters))))

(defmethod node-map-similar ((function   function)
                             (string     sequence)
                             (suffix     integer)
                             (node       interior-mixin)
                             (threshold  integer)
                             (characters vector))
  (declare (type simple-string                                string)
           (type a:array-index                                suffix)
           (type a:array-index                                threshold)
           (type (and (not simple-array) (array character 1)) characters))
  (assert (not (minusp threshold)))
  ;; First look at the keys of all children and determine which
  ;; children can be traversed within the remaining THRESHOLD.
  (let ((string-length (length string)))
    (labels ((visit-child (child suffix threshold)
               (values (node-map-similar
                        function string suffix child threshold characters)))
             (consider-child/char (key child suffix threshold)
               ;; The key is a single character try the three edit
               ;; operations if THRESHOLD permits.
               (declare (type a:array-index suffix threshold))
               ;; "insert" edit
               (when (plusp threshold)
                 (with-character (key characters)
                   (visit-child child suffix (1- threshold))))
               ;; "delete" edit
               (when (plusp threshold)
                 (let ((new-suffix (max 0 (1- suffix))))
                   (consider-child/char key child new-suffix (1- threshold))))
               ;; match or "change" edit
               (cond ((<= suffix 0))
                     ((let ((offset (- string-length suffix)))
                        (char= key (aref string offset)))
                      (with-character (key characters)
                        (visit-child child (1- suffix) threshold)))
                     ((plusp threshold)
                      (with-character (key characters)
                        (visit-child child (1- suffix) (1- threshold))))))
             (consider-child/string (key child suffix threshold)
               (let ((key-length (length key))
                     offset)
                 ;; The key is a string. Try the three edit operations
                 ;; for each character where THRESHOLD permits.
                 (declare (type simple-string key))
                 (labels
                     ((rec (index suffix threshold)
                        (declare (type a:array-index suffix threshold))
                        (cond ((= index key-length)
                               (visit-child child suffix threshold))
                              ((progn
                                 (setf offset (- string-length suffix))
                                 (and (plusp suffix)
                                      (char= (aref key index)
                                             (aref string offset))))
                               (with-character ((aref key index) characters)
                                 (rec (1+ index) (1- suffix) threshold)))
                              ((plusp threshold)
                               ;; "insert" character into query string
                               (with-character ((aref key index) characters)
                                 (rec (1+ index) suffix (1- threshold)))
                               ;; "delete" character from query string
                               (when (plusp suffix)
                                 (rec index (1- suffix) (1- threshold)))
                               ;; match or "change" edit
                               (when (plusp suffix)
                                 (with-character ((aref key index) characters)
                                   (rec (1+ index) (1- suffix) (1- threshold))))))))
                   (declare (dynamic-extent #'rec))
                   (rec 0 suffix threshold))))
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
      (map-children #'consider-child node (%children node))))
  ;; Now that children and compact entries have been handled, if NODE
  ;; is also a leaf, call the next method which is the one specialized
  ;; to `leaf-mixin' to handle the non-compact entries.
  (when (typep node 'leaf-mixin)
    (call-next-method)))
