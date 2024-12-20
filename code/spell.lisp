(defpackage #:spell
  (:use #:cl)
  (:export #:english-lookup
           #:english-check-paragraph))
(in-package #:spell)

(defgeneric lookup (string dictionary))
(defgeneric insert (object string dictionary))

(defclass node () ())

(defmethod make-load-form ((object node) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defvar *dictionary*)

(defclass dictionary ()
  ((%contents :initform (make-instance 'node) :accessor contents)))

(defmethod make-load-form ((object dictionary) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defun load-dictionary (filename)
  (with-open-file (stream filename)
    (let* ((counter 0)
           (*dictionary* (make-instance 'dictionary)))
      (do ((line (read-line stream nil stream)
                 (read-line stream nil stream)))
          ((eq stream line))
        (unless (eq #\; (aref line 0))
          (let ((string (concatenate 'string "(" line ")")))
            (destructuring-bind
                (spelling &rest args &key type &allow-other-keys)
                (read-from-string string)
              (remf args :type)
              (let ((word (apply #'word spelling type args)))
                (insert word spelling *dictionary*)))
            (incf counter))))
      (values *dictionary* counter))))

(defmethod lookup ((string string) (dictionary dictionary))
  (assert (plusp (length string)))
  (%lookup string (length string) (contents dictionary)))

(defgeneric entries (node))

(defgeneric %lookup (string suffix node)
  (:method ((string string) (suffix t) (node t))
    '()))

(defclass leaf-mixin ()
  ((%entries :initform '() :initarg :entries :accessor entries)))

(defmethod %lookup ((string string) (suffix (eql 0)) (node leaf-mixin))
  (entries node))

(defclass interior-mixin ()
  ((%children :initform '() :initarg :children :accessor children)))

(defclass interior-node (interior-mixin node) ())

(defmethod %lookup ((string string) (suffix (eql 0)) (node interior-node))
  '())

(defmethod %lookup ((string string) (suffix integer) (node interior-mixin))
  (let* ((character (aref string (- (length string) suffix)))
         (child     (find-child character (children node))))
    (if (null child)
        nil
        (%lookup string (1- suffix) child))))

(defclass leaf-node (leaf-mixin node) ())
(defclass interior-leaf-node (interior-mixin leaf-mixin node) ())

(defgeneric %insert (object string suffix node))

(defmethod %insert
    ((object t) (string string) (suffix (eql 0)) (node   leaf-mixin))
  (push object (entries node)))

(defmethod %insert ((object t) (string string) (suffix (eql 0)) (node node))
  (change-class node 'leaf-node)
  (%insert object string 0 node))

(defmethod %insert
    ((object t) (string string) (suffix (eql 0)) (node interior-node))
  (change-class node 'interior-leaf-node)
  (%insert object string 0 node))

(defmethod %insert
    ((object t) (string string) (suffix integer) (node leaf-mixin))
  (change-class node 'interior-leaf-node)
  (%insert object string suffix node))

(defmethod %insert ((object t) (string string) (suffix integer) (node node))
  (change-class node 'interior-node)
  (%insert object string suffix node))

(defmethod %insert
    ((object t) (string string) (suffix integer) (node interior-mixin))
  (let ((child (find-child (aref string (- (length string) suffix))
                           (children node))))
    (when (null child)
      (setf child (make-instance 'node))
      (setf (children node)
            (add-child child
                       (aref string (- (length string) suffix))
                       (children node))))
    (%insert object string (1- suffix) child)))

(defmethod insert ((object t) (string string) (dictionary dictionary))
  (%insert object string (length string) (contents dictionary)))

(defgeneric find-child (char entries))

(defmethod find-child ((char character) (entries list))
  (cdr (assoc char entries)))

(defmethod find-child ((char character) (entries vector))
  (let ((index (- (char-code char) #.(char-code #\a))))
    (aref entries index)))

(defgeneric add-child (node char entries))

(defmethod add-child ((node t) (char character) (entries list))
  (acons char node entries))

(defmethod add-child ((node t) (char character) (entries vector))
  (let ((index (- (char-code char) #.(char-code #\a))))
    (setf (aref entries index) node)))
