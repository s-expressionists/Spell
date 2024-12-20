(in-package #:spell)

(defvar *hash-table* (make-hash-table))

(defgeneric traverse (node) (:method-combination progn))

(defmethod traverse progn ((node node))
  (setf (gethash node *hash-table*) t))

(defmethod traverse progn ((node leaf-mixin))
  (loop for entry in (entries node)
        do (setf (gethash entry *hash-table*) t)))

(defmethod traverse progn ((node interior-mixin))
  (loop for child in (children node)
        do (traverse (cdr child))))

(defun analyze ()
  (let ((nodes 0)
        (leaf-nodes 0)
        (interior-nodes 0)
        (interior-leaf-nodes 0)
        (words 0))
    (flet ((fn (key value)
             (declare (ignore value))
             (etypecase key
               (word (incf words))
               (interior-leaf-node (incf interior-leaf-nodes))
               (interior-node (incf interior-nodes))
               (leaf-node (incf leaf-nodes))
               (node (incf nodes)))))
      (maphash #'fn *hash-table*)
      (list nodes leaf-nodes interior-nodes interior-leaf-nodes words))))
