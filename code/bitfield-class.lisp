(cl:in-package #:spell)

;;; Protocol

(defgeneric fields (class))

(defgeneric bitfield-slots (class))

;;; Bitfield utilities

(defun bitfield-type-p (type)
  (or (subtypep type '(or boolean unsigned-byte))
      (typep type '(cons (eql member)))))

(defvar *field-cache* (make-hash-table :test #'equal))

(defun field-for-type (type &key name-hint)
  (let* ((key      (cons bitfield::*bitfield-position* type))
         (cache    *field-cache*)
         (existing (gethash key cache)))
    (cond (existing
           (setf bitfield::*bitfield-position*
                 (bitfield:bitfield-slot-end existing))
           existing)
          (t
           (setf (gethash key cache)
                 (bitfield::parse-bitfield-slot (list name-hint type)))))))

(defun make-accessors (field total-size)
  (let* ((start        (bitfield:bitfield-slot-start field))
         (end          (bitfield:bitfield-slot-end   field))
         (size         (- end start))
         (integer-form `(ldb (byte ,size ,start)
                             (the (unsigned-byte ,total-size)
                                  (slot-value object '%info)))))
    (cons (compile nil `(lambda (object)
                          (declare (optimize (speed 3) (safety 0) (debug 0)))
                          ,(bitfield:bitfield-slot-unpack
                            field integer-form)))
          (compile nil `(lambda (new-value object)
                          (declare (optimize (speed 3) (safety 0) (debug 0)))
                          (setf ,integer-form
                                ,(bitfield:bitfield-slot-pack
                                  field 'new-value))
                          new-value)))))

(defvar *accessor-cache* (make-hash-table :test #'equal))

(defun accessors-for-field (field total-size)
  (destructuring-bind (reader . writer)
      (a:ensure-gethash (cons field total-size) *accessor-cache*
                        (make-accessors field total-size))
    (values reader writer)))

;;; `effective-field-slot-definition'

(defclass effective-field-slot-definition (c2mop:standard-effective-slot-definition)
  ((%field  :accessor field)
   (%reader :type     function
            :accessor reader)
   (%writer :type     function
            :accessor writer)))

;;; CCL does not support re-initializing slot definition
;;; metaobjects. So we inject the allocation here.
(defmethod initialize-instance :around
    ((instance effective-field-slot-definition) &rest initargs &key)
  (apply #'call-next-method instance
         (list* :allocation :bitfield
                (a:remove-from-plist initargs :allocation))))

;;; `bitfield-mixin'
;;;
;;; A mixin for class metaobjects. Adding this mixin to the
;;; superclasses of a class metaobject causes certain slots of
;;; instances of that class to be backed by a synthetic slot the value
;;; of which is an integer.

(defclass bitfield-mixin () ())

(defmethod bitfield-slots ((class bitfield-mixin))
  (let ((class (c2mop:ensure-finalized class)))
    (remove-if-not (a:of-type 'effective-field-slot-definition)
                   (c2mop:class-slots class))))

(defmethod fields ((class bitfield-mixin))
  (mapcar #'field (bitfield-slots class)))

(defmethod c2mop:compute-slots ((class bitfield-mixin))
  (let ((slots (call-next-method)))
    (if (some (a:of-type 'effective-field-slot-definition) slots)
        (list* (make-instance 'c2mop:standard-effective-slot-definition
                              :class        class
                              :name         '%info
                              :initargs     '(:info)
                              :initform     0
                              :initfunction (constantly 0))
               slots)
        slots)))

(defun compute-slot-field (slot-definition)
  (let ((name (c2mop:slot-definition-name slot-definition))
        (type (c2mop:slot-definition-type slot-definition)))
    (field-for-type type :name-hint name)))

(defmethod c2mop:compute-slots :around ((class bitfield-mixin))
  (let* ((slots       (call-next-method))
         (field-slots (remove-if-not
                       (a:of-type 'effective-field-slot-definition) slots)))
    (if (null field-slots)
        slots
        (let* ((fields     (let ((bitfield::*bitfield-position* 0))
                             (mapcar (lambda (slot)
                                       (let ((slot-for-type (field slot)))
                                         (setf (field slot)
                                               (compute-slot-field
                                                slot-for-type))))
                                     field-slots)))
               (total-size (bitfield:bitfield-slot-end (a:lastcar fields))))
          (mapc (lambda (slot)
                  (setf (values (reader slot) (writer slot))
                        (accessors-for-field (field slot) total-size)))
                field-slots)
          #+sbcl
          (let ((info-slot (find '%info slots :test #'eq
                                              :key  #'c2mop:slot-definition-name)))
            (setf (c2mop:slot-definition-type info-slot)
                  `(unsigned-byte ,total-size)))
          slots))))

(defmethod c2mop:effective-slot-definition-class ((class bitfield-mixin)
                                                   &rest initargs &key type)
  (declare (ignore initargs))
  (if (bitfield-type-p type)
      (find-class 'effective-field-slot-definition)
      (call-next-method)))

(defmethod c2mop:compute-effective-slot-definition
    ((class bitfield-mixin) (name t) (slot-definitions t))
  (let ((effective-slot (call-next-method)))
    (if (typep effective-slot 'effective-field-slot-definition)
        ;; If the type is a `member'-type, the implementation may have
        ;; reordered the elements in the type of EFFECTIVE-SLOT. Take
        ;; the (hopefully raw) slot type from the most specific direct
        ;; slot definition in that case.
        (let ((slot (if (typep (c2mop:slot-definition-type effective-slot)
                               '(cons (eql member)))
                        (first slot-definitions)
                        effective-slot)))
          (setf (field effective-slot) slot)
          effective-slot)
        effective-slot)))

;;; Slot access

(defmethod c2mop:slot-boundp-using-class
    ((class           bitfield-mixin)
     (object          t)
     (slot-definition effective-field-slot-definition))
  (slot-boundp object '%info))

(defmethod c2mop:slot-value-using-class
    ((class           bitfield-mixin)
     (object          t)
     (slot-definition effective-field-slot-definition))
  (funcall (reader slot-definition) object))

(defmethod (setf c2mop:slot-value-using-class)
    ((new-value       t)
     (class           bitfield-mixin)
     (object          t)
     (slot-definition effective-field-slot-definition))
  (funcall (writer slot-definition) new-value object))

;;; Convenience Metaclass

(defclass bitfield-class (bitfield-mixin standard-class) ())

(defmethod c2mop:validate-superclass ((class      bitfield-class)
                                      (superclass standard-class))
  t)
