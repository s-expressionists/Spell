(cl:defpackage #:spell.generate-documentation
  (:use
   #:cl)

  (:local-nicknames
   (#:a #:alexandria)))

(cl:in-package #:spell.generate-documentation)

;;; Utilities

(defun emit-section (title &key (level "section"))
  (format t "@node ~A~%@~A ~2:*~A~2%" title level))

(defun emit-string (string stream)
  (loop :for character :across string
        :do (case character
              (#\{ (write-string "@{" stream))
              (#\} (write-string "@}" stream))
              (#\@ (write-string "@@" stream))
              (t   (write-char character stream)))))

;;; Word class descriptions

(defun relevant-direct-subclasses (class)
  (let* ((all         (sb-mop:class-direct-subclasses class))
         (interesting (remove-if (lambda (class)
                                   (or (subtypep class 'spell::implicit-base-mixin)
                                       (subtypep class 'spell::explicit-base-mixin)))
                                 all) ))
    (sort interesting #'string< :key #'class-name)))

(defun all-word-classes ()
  (let ((result '()))
    (labels ((visit (class)
               (pushnew class result :test #'eq)
               (mapc #'visit (relevant-direct-subclasses class))))
      (visit (find-class 'spell::word)))
    (reverse result)))

(defun emit-word-class-graph (stream)
  (format stream "@example~%")
  (let ((utilities.print-tree:*use-unicode?* nil))
    (utilities.print-tree:print-tree
     stream (find-class 'spell::word)
     (utilities.print-tree:make-node-printer
      (lambda (stream depth node)
        (declare (ignore depth))
        (let ((name (class-name node)))
          (format stream "~S" name)
                                        ; (format stream "@ref{class-~(~A~),~:*~(~S~)}" name)
          ))
      nil
      #'relevant-direct-subclasses)))
  (format stream "~&@end example~%"))

(defun emit-word-class-description (class stream)
  (let* ((superclasses     (sb-mop:class-direct-superclasses class))
         (name             (class-name class))
         (superclass-names (mapcar #'class-name superclasses))
         (documentation    (documentation class t)))
    (format stream "~2%@deftp @symbol{~(~A~),~(~A~)} (~{~A~^ ~})~% "
            (symbol-name name) (package-name (symbol-package name)) superclass-names)
    (if documentation
        (emit-string documentation stream)
        (format stream "@emph{not documented}~%"))
    (format stream "@end deftp~%")))

(defun emit-word-class-descriptions ()
  (a:with-output-to-file (stream "generated-word-classes.texi"
                                            :if-exists :supersede)
    (emit-word-class-graph stream)
    (mapc (a:rcurry #'emit-word-class-description stream) (all-word-classes))))

(emit-word-class-descriptions)
