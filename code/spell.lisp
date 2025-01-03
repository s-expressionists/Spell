(in-package #:spell)

;;; Internal protocols

(defgeneric %lookup (string suffix node))

(defgeneric %insert (object string suffix node))

;;; Node classes

(defclass node () ())

(defmethod make-load-form ((object node) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod %insert ((object t) (string string) (suffix (eql 0)) (node node))
  (change-class node 'leaf-node)
  (%insert object string 0 node))

(defmethod %insert ((object t) (string string) (suffix integer) (node node))
  (change-class node 'interior-node)
  (%insert object string suffix node))

(defclass leaf-mixin ()
  ((%entries :initform '() :initarg :entries :accessor entries)))

(defmethod %lookup ((string string) (suffix (eql 0)) (node leaf-mixin))
  (entries node))

(defmethod %insert
    ((object t) (string string) (suffix (eql 0)) (node leaf-mixin))
  (push object (entries node)))

(defmethod %insert
    ((object t) (string string) (suffix integer) (node leaf-mixin))
  (change-class node 'interior-leaf-node)
  (%insert object string suffix node))

(defclass interior-mixin ()
  ((%children :initform '() :initarg :children :accessor children)))

(defmethod %lookup ((string string) (suffix integer) (node interior-mixin))
  (let* ((character (aref string (- (length string) suffix)))
         (child     (find-child character (children node))))
    (if (null child)
        nil
        (%lookup string (1- suffix) child))))

(defmethod %insert
    ((object t) (string string) (suffix integer) (node interior-mixin))
  (let* ((character (aref string (- (length string) suffix)))
         (children  (children node))
         (child     (find-child character children)))
    (when (null child)
      (setf child           (make-instance 'node)
            (children node) (add-child child character children)))
    (%insert object string (1- suffix) child)))

(defclass interior-node (interior-mixin node) ())

(defmethod %lookup ((string string) (suffix (eql 0)) (node interior-node))
  '())

(defmethod %insert
    ((object t) (string string) (suffix (eql 0)) (node interior-node))
  (change-class node 'interior-leaf-node)
  (%insert object string 0 node))

(defclass leaf-node (leaf-mixin node) ())

(defclass interior-leaf-node (interior-mixin leaf-mixin node) ())

;;; Child node access methods

(defmethod find-child ((char character) (entries list))
  (cdr (assoc char entries)))

(defmethod add-child ((node t) (char character) (entries list))
  (acons char node entries))

;;; Dictionary

(defclass dictionary ()
  ((%contents    :accessor contents
                 :initform (make-instance 'node))
   (%entry-count :accessor entry-count
                 :initform 0)))

(defmethod make-load-form ((object dictionary) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod lookup ((string string) (dictionary dictionary))
  (assert (plusp (length string)))
  (%lookup string (length string) (contents dictionary)))

(defmethod insert ((object t) (string string) (dictionary dictionary))
  (%insert object string (length string) (contents dictionary)))

(defmethod load-dictionary ((source stream)
                            &key (into (make-instance 'dictionary)))
  (let ((count 0))
    (map-dictionary-file-entries
     (lambda (spelling type base &rest initargs)
       (let ((word (apply #'word spelling type :base base initargs)))
         (insert word spelling into))
       (incf count))
     source)
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
