(cl:in-package #:spell)

;;; `word-class' metaclass

(defclass word-class (bitfield-class) ())

;;; `word' base class

(defclass word (utilities.print-items:print-items-mixin)
  ((%base :initarg :base
          :reader  base))
  (:metaclass word-class))

(defmethod make-load-form ((object word) &optional environment)
  ;; Do not save slots that are stored in the `%info' bitfield slot.
  (let* ((class      (class-of object))
         (save-slots (remove-if (a:of-type 'effective-field-slot-definition)
                                (c2mop:class-slots class)))
         (save-slots (mapcar #'c2mop:slot-definition-name save-slots)))
    (make-load-form-saving-slots object :slot-names  save-slots
                                        :environment environment)))

(defmethod utilities.print-items:print-items append ((object word))
  (append
   `((:base "~S" ,(base object)))
   (when (and (slot-exists-p object '%info)
              (slot-boundp object '%info))
     (loop :with class    =   (class-of object)
           :for  previous =   :base :then name
           :for  field    :in (fields class)
           :for  name     =   (bitfield:bitfield-slot-name field)
           :collect `((,name (:after ,previous))
                      " ~(~A~):~A"
                      ,(subseq (string name) 1)
                      ,(ignore-errors (slot-value object name)))))))

;;; Word class registry

;;; A vector of entries of the form
;;;
;;;   (TYPE CONSTRUCTOR CLASS)
;;;
;;; where
;;;
;;; TYPE is the keyword which identifies the word type in the
;;; dictionary data.
;;;
;;; CONSTRUCTOR is a function of two arguments INFO and BASE which
;;; makes and returns an instance of the word class with the supplied
;;; info and base.
;;;
;;; CLASS is the name of the class that should be used to represent
;;; words of the given TYPE.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +word-class-index-bits+ 5)

  (defconstant +word-class-index-limit+ (ash 1 +word-class-index-bits+)))

(deftype word-class-index () `(unsigned-byte ,+word-class-index-bits+))

(declaim (type (simple-array t (#.+word-class-index-limit+)) *word-classes*))
(defparameter *word-classes*
  (make-array +word-class-index-limit+ :initial-element nil))

(defun find-word-class (type)
  (or (find type *word-classes* :test #'eq :key #'first)
      (error "~@<Could not find word class for type ~S.~@:>" type)))

(defun make-word-class-constructor (class-name)
  (let* ((class (c2mop:ensure-finalized (find-class class-name)))
         (infop (not (null (fields class)))))
    (compile nil `(lambda (info base)
                    ,@(when (not infop) `((declare (ignore info))))
                    (make-instance ',class-name
                                   :base base
                                   ,@(when infop '(:info info)))))))

(defun add-word-class (type class-name)
  (let* ((constructor (make-word-class-constructor class-name))
         (entry       (list type constructor class-name))
         (classes     *word-classes*)
         (index       (position nil classes)))
    (setf (aref classes index) entry)))

(defmacro defword (class-name superclasses slots)
  (let ((type (intern (symbol-name class-name) '#:keyword)))
    `(progn
       (defclass ,class-name (,@superclasses word)
         (,@slots)
         (:metaclass word-class))

       (add-word-class ',type ',class-name))))

;;; Making word instances

(defun make-word (spelling type &rest initargs &key &allow-other-keys)
  (declare (ignore spelling))
  (let* ((class-info (find-word-class type))
         (class      (third class-info))
         (initargs   (loop :for (key value) :on initargs :by #'cddr
                           :collect key
                           :collect (if (stringp value)
                                        (intern-string value)
                                        value))))
    (apply #'make-instance class initargs)))
