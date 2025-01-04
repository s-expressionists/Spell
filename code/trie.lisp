(cl:in-package #:spell)

;;; Internal protocols

(defgeneric %lookup (string suffix node))

(defgeneric %insert (object string suffix node))

;;; Node classes

(defclass node (utilities.print-items:print-items-mixin) ())

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

(defmethod utilities.print-items:print-items append ((object leaf-mixin))
  (let ((entries (map 'list (lambda (entry)
                              (class-name (class-of entry)))
                      (entries object))))
    `(((:entries (:after :children)) " entries: ~{~A~^ ~}" ,entries))))

(defmethod %lookup ((string string) (suffix (eql 0)) (node leaf-mixin))
  (mapcar (lambda (word)
            ;; This is temporary: make an instance of the explicit
            ;; base "sibling" class.
            (if (typep word 'implicit-base-mixin)
                (let* ((class-name  (class-name (class-of word)))
                       (class-info  (find class-name *word-classes*
                                          :test #'eq :key #'third))
                       (new-class   (find-class (fourth class-info)))
                       (base-suffix (base-suffix word))
                       (base        (if (plusp base-suffix)
                                        (subseq string 0 (- (length string)
                                                            base-suffix))
                                        string)))
                  (if (slot-exists-p (c2mop:class-prototype new-class) '%info)
                      (let ((new-info (ldb (byte 29 +base-suffix-bits+)
                                           (slot-value word '%info))))
                        (make-instance new-class :info new-info
                                                 :base base))
                      (make-instance new-class :base base)))
                word))
          (entries node)))

(defmethod %insert
    ((object t) (string string) (suffix (eql 0)) (node leaf-mixin))
  (push object (entries node)))

(defmethod %insert
    ((object t) (string string) (suffix integer) (node leaf-mixin))
  (change-class node 'interior-leaf-node)
  (%insert object string suffix node))

(defclass interior-mixin ()
  ((%children :initform '() :initarg :children :accessor children)))

(defmethod utilities.print-items:print-items append ((object interior-mixin))
  (let ((child-count (length (children object))))
    `((:children "~D ~:*child~[ren~;~:;ren~]" ,child-count))))

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
