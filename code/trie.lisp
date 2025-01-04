(cl:in-package #:spell)

;;; `node' class

(defclass node (utilities.print-items:print-items-mixin) ())

(defmethod make-load-form ((object node) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod %insert ((object t) (string string) (suffix (eql 0)) (node node))
  (change-class node 'leaf-node)
  (%insert object string 0 node))

(defmethod %insert ((object t) (string string) (suffix integer) (node node))
  (change-class node 'interior-node)
  (%insert object string suffix node))

;;; `leaf-mixin' class and leaf node protocol method

(defclass leaf-mixin ()
  ((%entries :accessor %entries
             :initform '())))

(defmethod utilities.print-items:print-items append ((object leaf-mixin))
  (let ((entries '()))
    (map-entries (lambda (entry)
                   (push (class-name (class-of entry)) entries))
                 object  (%entries object))
    `(((:entries (:after :children)) " entries: ~{~A~^ ~}" ,entries))))

(defmethod map-entries ((function function) (node leaf-mixin) (entries list))
  (mapc function entries))

(defmethod add-entry ((entry t) (node leaf-mixin) (entries list))
  (list* entry entries))

(defmethod %lookup
    ((function function) (string string) (suffix (eql 0)) (node leaf-mixin))
  (map-entries
   (lambda (word)
     ;; This is temporary: make an instance of the explicit base
     ;; "sibling" class.
     (let ((word (if (typep word 'implicit-base-mixin)
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
                     word)))
       (funcall function word)))
   node (%entries node)))

(defmethod %insert
    ((object t) (string string) (suffix (eql 0)) (node leaf-mixin))
  (setf (%entries node) (add-entry object node (%entries node))))

(defmethod %insert
    ((object t) (string string) (suffix integer) (node leaf-mixin))
  (change-class node 'interior-leaf-node)
  (%insert object string suffix node))

;;; `interior-mixin' class and interior node protocol methods

(defclass interior-mixin ()
  ((%children :accessor %children
              :initform '())))

(defmethod utilities.print-items:print-items append ((object interior-mixin))
  (let ((child-count 0))
    (map-children (lambda (key child)
                    (declare (ignore key child))
                    (incf child-count))
                  object (%children object))
    `((:children "~D ~:*child~[ren~;~:;ren~]" ,child-count))))

(defmethod map-children
    ((function function) (node interior-mixin) (children list))
  (mapc (lambda (cell)
          (destructuring-bind (key . child) cell
            (funcall function key child)))
        children))

(defmethod find-child ((char character) (node interior-mixin) (children list))
  (cdr (assoc char children)))

(defmethod add-child
    ((char character) (child t) (node interior-mixin) (children list))
  (acons char child children))

(defmethod %lookup
    ((function function) (string string) (suffix integer) (node interior-mixin))
  (let* ((character (aref string (- (length string) suffix)))
         (child     (find-child character node (%children node))))
    (when (not (null child))
      (%lookup function string (1- suffix) child))))

(defmethod %insert
    ((object t) (string string) (suffix integer) (node interior-mixin))
  (let* ((character (aref string (- (length string) suffix)))
         (children  (%children node))
         (child     (find-child character node children)))
    (when (null child)
      (setf child            (make-instance 'node)
            (%children node) (add-child character child node children)))
    (%insert object string (1- suffix) child)))

;;; Concrete node classes

(defclass interior-node (interior-mixin node) ())

(defmethod %lookup
    ((function function) (string string) (suffix (eql 0)) (node interior-node))
  nil)

(defmethod %insert
    ((object t) (string string) (suffix (eql 0)) (node interior-node))
  (change-class node 'interior-leaf-node)
  (%insert object string 0 node))

(defclass leaf-node (leaf-mixin node) ())

(defclass interior-leaf-node (interior-mixin leaf-mixin node) ())
