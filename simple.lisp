(in-package #:spell)

(defun word (spelling type &rest initargs &key &allow-other-keys)
  (declare (ignore spelling type initargs))
  t)

(defmethod %insert (object string (suffix (eql 0)) (node leaf-mixin))
  (setf (entries node) t))
