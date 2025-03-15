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
