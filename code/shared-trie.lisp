(cl:in-package #:spell)

;;; Internal protocol

(defgeneric share-structure (node &key depth-limit))

(defgeneric share-key (node index)
  (:method-combination append))

(defgeneric index-node (node index &key depth-limit))

(defgeneric share-structure-2 (node index))

;;; Top-level method

(defmethod share-structure ((node compact-node) &key (depth-limit 3))
  (let ((index (make-instance 'share-index)))
    ;; Index sub-trees of depth less than DEPTH-LIMIT. The DEPTH-LIMIT
    ;; limits the space needed for the index and the time for
    ;; computing "share keys".
    (index-node node index :depth-limit depth-limit)
    ;; Recognize equivalent sub-trees and replace all instances with
    ;; the respective canonical instance.
    (share-structure-2 node index)))

;;; Keys and indexing

(defclass share-index ()
  ((%key->node :reader   key->node
               :initform (make-hash-table :test #'equal))
   (%node->key :reader   node->key
               :initform (make-hash-table :test #'eq))))

(defmethod share-key :around ((node t) (index share-index))
  (a:ensure-gethash node (node->key index) (call-next-method)))

;;; This function is called for node objects that are instances of
;;; subclasses of `interior-mixin' since leaf nodes are replaced with
;;; cons-based structures by `compact'.
(defmethod share-key append ((node compact-interior-mixin) (index share-index))
  (let ((result '()))
    (map-children (lambda (key child)
                    (setf result (list* key (typecase child
                                              (compact-node
                                               (share-key child index))
                                              (compact-entry
                                               child)
                                              (t ; `vector-of-compact-entry'
                                               (coerce child 'list)))
                                        result))
                    nil)
                  node (%children node))
    result))

(defmethod share-key append ((node compact-leaf-mixin) (index share-index))
  (let ((result '()))
    (map-leaf-entries (lambda (entry) (push entry result))
                      node (%entries node))
    result))

(defun find-node (node index)
  (a:when-let* ((key   (gethash node (node->key index)))
                (value (gethash key (key->node index))))
    (when (not (eq node value))
      value)))

(defun register-node (node index)
  (a:ensure-gethash (share-key node index) (key->node index) node))

;;; Phase 1

(defmethod index-node ((node compact-interior-mixin) (index t)
                       &key (depth-limit 3))
  (let ((max-depth 0))
    (map-children (lambda (key child)
                    (declare (ignore key))
                    (when (typep child 'compact-node)
                      (a:maxf max-depth (index-node child index)))
                    nil)
                  node (%children node))
    (let ((subtree-depth (1+ max-depth)))
      (when (<= subtree-depth depth-limit)
        (register-node node index))
      subtree-depth)))

;;; Phase 2

(defmethod share-structure-2 ((node node) (index t))
  (or (find-node node index) node))

(defmethod share-structure-2 ((node interior-mixin) (index t))
  (or (find-node node index)
      (progn
        (map-children (lambda (key child)
                        (if (typep child 'compact-node)
                            (values key (share-structure-2 child index))
                            nil))
                      node (%children node))
        node)))
