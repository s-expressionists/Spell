(cl:in-package #:spell)

;;; `word-class' metaclass

(defclass word-class (bitfield-class) ())

(defmethod c2mop:compute-slots ((class word-class))
  ;; Move `%base-suffix' to the front so that it uses predictable bits
  ;; in the bitfield. But the `%info' slot has to be initialized
  ;; first, so keep that at the very front.
  (let ((slots (call-next-method)))
    (flet ((slot-position (name)
             (position name slots :test #'eq :key #'c2mop:slot-definition-name)))
      (let ((info-position        (slot-position '%info))
            (base-suffix-position (slot-position '%base-suffix)))
        (if base-suffix-position
            (append (subseq slots 0 (1+ info-position))
                    (list (nth base-suffix-position slots))
                    (subseq slots (1+ info-position) base-suffix-position)
                    (subseq slots (1+ base-suffix-position)))
            slots)))))

;;; `word' base class

(defclass word (utilities.print-items:print-items-mixin)
  ()
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
  (when (and (slot-exists-p object '%info)
             (slot-boundp object '%info))
    (loop :with class    =   (class-of object)
          :for  previous =   :base :then name
          :for  field    :in (fields class)
          :for  name     =   (bitfield:bitfield-slot-name field)
          :collect `((,name (:after ,previous))
                     " ~(~A~):~A"
                     ,(subseq (string name) 1)
                     ,(ignore-errors (slot-value object name))))))

(defclass explicit-base-mixin ()
  ((%base :initarg :base
          :type    string
          :reader  base))
  (:default-initargs
   :base (a:required-argument :base))
  (:metaclass bitfield-class))

(defmethod print-items:print-items append ((object explicit-base-mixin))
  `((:base "~S" ,(base object))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +base-suffix-bits+ 3))

(defconstant +base-suffix-limit+
  (1- (ash 1 +base-suffix-bits+)))

(defclass implicit-base-mixin ()
  ((%base-suffix :initarg :base-suffix
                 :type    (unsigned-byte #.+base-suffix-bits+)
                 :reader  base-suffix))
  (:metaclass bitfield-class))

;;; Word class registry

;;; A vector of entries of the form
;;;
;;;   (TYPE CONSTRUCTOR IMPLICIT-BASE-CLASS EXPLICIT-BASE-CLASS)
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
;;; EXPLICIT-BASE-CLASS and IMPLICIT-BASE-CLASS are the names of the
;;; classes that should be used to represent words of the given TYPE
;;; when it is necessary and not necessary respectively to explicitly
;;; store the base string.
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

(defun add-word-class (type implicit-base-class-name explicit-base-class-name)
  (let* ((constructor (make-word-class-constructor explicit-base-class-name))
         (entry       (list type
                            constructor
                            implicit-base-class-name
                            explicit-base-class-name))
         (classes     *word-classes*)
         (index       (position nil classes)))
    (setf (aref classes index) entry)))

(defmacro defword (class-name superclasses slots)
  (let ((type                     (a:make-keyword class-name))
        (implicit-base-class-name (a:symbolicate '#:implicit-base- class-name))
        (explicit-base-class-name (a:symbolicate '#:explicit-base- class-name)))
    `(progn
       (defclass ,class-name (,@superclasses word)
         (,@slots)
         (:metaclass word-class))

       (defclass ,implicit-base-class-name (implicit-base-mixin ,class-name)
         ()
         (:metaclass word-class))

       (defclass ,explicit-base-class-name (explicit-base-mixin ,class-name)
         ()
         (:metaclass word-class))

       (add-word-class
        ',type ',implicit-base-class-name ',explicit-base-class-name))))

;;; Making word instances

(defun make-word (spelling type base &rest initargs &key &allow-other-keys)
  (let ((initargs   (loop :for (key value) :on initargs :by #'cddr
                          :collect key
                          :collect (if (stringp value)
                                       (intern-string value)
                                       value)))
        (class-info (find-word-class type)))
    (destructuring-bind (implicit-base-class explicit-base-class)
        (cddr class-info)
      (let (base-suffix)
        (cond ((string= spelling base)
               (apply #'make-instance implicit-base-class initargs))
              ((and (<= (setf base-suffix (- (length spelling) (length base)))
                        +base-suffix-limit+)
                    (a:starts-with-subseq base spelling))
               (apply #'make-instance implicit-base-class
                      :base-suffix base-suffix initargs))
              (t
               (apply #'make-instance explicit-base-class
                      :base base initargs)))))))
