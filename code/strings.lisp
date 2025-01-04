(cl:in-package #:spell)

;;; Coercion to base string

(defun base-stringify (string)
  (if (every (a:of-type 'base-char) string)
      (coerce string 'simple-base-string)
      string))

;;; String interning

(defvar *strings*)

(defmacro with-string-interning (() &body body)
  `(let ((*strings* (make-hash-table :test #'equal)))
     ,@body))

(defun intern-string (string)
  (a:ensure-gethash string *strings* (base-stringify string)))
