(cl:in-package #:spell)

;;; Internal protocol

(defgeneric compact (dictionary))

(defgeneric compact-node (node depth))

(defgeneric compact-node-slots (node depth)
  (:method-combination append))

(defgeneric compact-entry (entry))

;;; `compact-node'

(defclass compact-node (node) ())

;;; `compact-leaf-mixin', related types and leaf protocol methods

(deftype compact-entry ()
  'word)

(defun %every-compact-entry (object)
  (and (typep object 'sequence) (every (a:of-type 'compact-entry) object)))

(deftype vector-of-compact-entry ()
  '(and (simple-array * 1) (satisfies %every-compact-entry)))

(deftype compact-entries ()
  'vector-of-compact-entry)

(defclass compact-leaf-mixin (leaf-mixin)
  ((%entries :initarg :entries
             :type    compact-entries)))

(defmethod utilities.print-items:print-items append
    ((object compact-leaf-mixin))
  (let ((entries '()))
    (map-entries (lambda (entry) (push entry entries))
                 object  (%entries object))
    `(((:entries (:after :children)) " entries: ~{~A~^ ~}" ,entries))))

(defmethod map-entries
    ((function function) (node compact-leaf-mixin) (entries vector))
  (map nil function entries))

(defmethod %lookup ((function function)
                    (string   string)
                    (suffix   (eql 0))
                    (node     compact-leaf-mixin))
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

(defmethod compact-node-slots append ((node leaf-mixin) (depth integer))
  (flet ((compact-entry (entries) entries))
    (let ((new-entries '()))
      (map-entries (lambda (entry)
                     (push (compact-entry entry) new-entries))
                   node (%entries node))
      (list :entries (coerce new-entries 'vector)))))

;;; `compact-interior-mixin'

(deftype child-key ()
  'character)

(defun %every-compact-child-chell (object)
  (and (typep object 'sequence)
       (let ((length (length object)))
         (and (zerop (mod length 2))
              (loop :for i :below length :by 2
                    :for key   = (aref object (+ i 0))
                    :for child = (aref object (+ i 1))
                    :always (and (typep key 'child-key)
                                 (typep child '(or null ; can happen temporarily while rebuilding from fasl
                                                   node))))))))

(deftype vector-of-compact-child-cell ()
  '(and (simple-array t 1) (satisfies %every-compact-child-chell)))

(deftype compact-child-cells ()
  'vector-of-compact-child-cell)

(defclass compact-interior-mixin (interior-mixin)
  ((%children :initarg  :children
              :type     compact-child-cells)))

(defmethod map-children
    ((function function) (node compact-interior-mixin) (children vector))
  (loop :for i     :below (length children) :by 2
        :for key   =      (aref children (+ i 0))
        :for child =      (aref children (+ i 1))
        :do (funcall function key child)))

(defmethod find-child
    ((char character) (node compact-interior-mixin) (children vector))
  (loop :for i   :below (length children) :by 2
        :for key =      (aref children i)
        :when (char= key char)
          :do (return (aref children (1+ i)))))

(defmethod %lookup ((function function)
                    (string   string)
                    (suffix   integer)
                    (node     compact-interior-mixin))
  (let* ((character (aref string (- (length string) suffix)))
         (child     (find-child character node (%children node))))
    (when (not (null child))
      (%lookup function string (1- suffix) child))))

(defmethod compact-node-slots append ((node raw-interior-mixin) (depth integer))
  (flet ((compact-child (key child)
           (cons key (compact-node child (1+ depth)))))
    (let ((compact-children '()))
      (map-children (lambda (key child)
                      (push (compact-child key child) compact-children))
                    node (%children node))
      ;; Sorting is essential for the structure sharing optimization.
      (setf compact-children (sort compact-children #'string< :key #'car))
      (let ((new-children (make-array (* 2 (length compact-children)))))
        (loop :for (key . child) :in compact-children
              :for i :from 0 :by 2
              :do (setf (aref new-children (+ i 0)) key
                        (aref new-children (+ i 1)) child))
        (list :children new-children)))))

;;; Concrete node classes

(defclass compact-interior-node (compact-interior-mixin compact-node) ())

(defmethod %lookup ((function function)
                    (string   string)
                    (suffix   (eql 0))
                    (nodef    compact-interior-node))
  nil)

(defmethod compact-node ((node raw-interior-node) (depth integer))
  (apply #'make-instance 'compact-interior-node
         (compact-node-slots node depth)))

(defclass compact-leaf-node (compact-leaf-mixin compact-node) ())

(defmethod compact-node ((node raw-leaf-node) (depth integer))
  (apply #'make-instance 'compact-leaf-node
         (compact-node-slots node depth)))

(defclass compact-interior-leaf-node (compact-interior-mixin
                                      compact-leaf-mixin
                                      compact-node)
  ())

(defmethod compact-node ((node raw-interior-leaf-node) (depth integer))
  (apply #'make-instance 'compact-interior-leaf-node
         (compact-node-slots node depth)))
