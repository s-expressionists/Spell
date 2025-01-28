(cl:in-package #:spell)

;;; `node' class

(defclass node (utilities.print-items:print-items-mixin) ())

;;; Occurs only in empty tries (raw and compact).
(macrolet ((define (suffix)
             `(defmethod %lookup ((function function)
                                  (string   string)
                                  (suffix   ,suffix)
                                  (node     node))
                nil)))
  (define (eql 0))
  (define t))

(defmethod make-load-form ((object node) &optional environment)
  (make-load-form-saving-slots object :environment environment))

;;; `leaf-mixin' class

(defclass leaf-mixin ()
  ((%entries :reader   %entries
             :initform #())))

;;; `interior-mixin' class

(defclass interior-mixin ()
  ((%children :reader   %children
              :initform #())))

(defmethod utilities.print-items:print-items append ((object interior-mixin))
  (let ((child-count 0))
    (map-children (lambda (key child)
                    (declare (ignore key child))
                    (incf child-count))
                  object (%children object))
    `((:children "~D ~:*child~[ren~;~:;ren~]" ,child-count))))

;;; Concrete node classes

(defclass interior-node (interior-mixin node) ()) ; TODO: are these useful?

(defclass leaf-node (leaf-mixin node) ())

(defclass interior-leaf-node (interior-mixin leaf-mixin node) ())
