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

(defmethod relevant-generic-functions ((class t))
  (sb-mop:specializer-direct-generic-functions class))

(defmethod relevant-generic-functions ((class (eql (find-class 'spell::word))))
  (list #'spell:base))

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

(defun emit-generic-function-description (generic-function stream
                                          &key reference?)
  (let ((name (sb-mop:generic-function-name generic-function)))
    (if reference?
        (format stream "~2%The generic function ~
                           @ref{Generic-Function ~(~A~)|~(~A~),~:*~(~A~)} ~
                           can be applied to words of this class.~%"
                (package-name (symbol-package name)) (symbol-name name))
        (let ((documentation (documentation generic-function t)))
          (format stream "~2%@defgena{~(~A~),~(~A~)} (~{~(~A~)~^ ~})~%"
                  (symbol-name name) (package-name (symbol-package name))
                  (sb-mop:generic-function-lambda-list generic-function))
          (if documentation
              (emit-string documentation stream)
              (format stream "@emph{not documented}"))
          (format stream "~%@end deffn~%")))))

(defun find-example-words (class)
  (format *trace-output* "Class ~A~%" class)
  (loop :with result = '()
        :with i = 0
        :for threshold :from 5 :to 9
        :do (format *trace-output* "  Threshold ~D~%" threshold)
        :do (spell::map-similar
             (lambda (word spelling distance)
               (declare (ignore distance))
               (when (typep word class)
                 (when (and (member class
                                    (sb-mop:class-direct-superclasses (class-of word))
                                    :test #'eq)
                            (not (find #\' spelling))
                            (or (not (member class (list (find-class 'spell::noun)
                                                         (find-class 'spell::verb))))
                                (and (> (length spelling) 5)
                                     (zerop (mod (incf i) 7)))))
                   (push (cons spelling word) result))))
             "a" spell::*english-dictionary* threshold)
        :when (>= (length result) 3)
          :do (return (subseq result 0 3))
        :finally (return (subseq result 0 (min (length result) 3)))))

(defvar *examples*
  (mapcar #'find-example-words (remove (find-class 'spell::word) (all-word-classes))))

(defun emit-word-class-description (class stream seen-generic-functions)
  (let* ((superclasses     (sb-mop:class-direct-superclasses class))
         (name             (class-name class))
         (superclass-names (mapcar #'class-name superclasses))
         (documentation    (documentation class t)))
    (format stream "~2%@defclassa{~(~A~),~(~A~)} (~{~A~^ ~})~% "
            (symbol-name name) (package-name (symbol-package name)) superclass-names)
    (if documentation
        (emit-string documentation stream)
        (format stream "@emph{not documented}~%"))

    (format stream "@end deftp~%")

    (mapc (lambda (generic-function)
            (let ((reference? (nth-value
                               1 (a:ensure-gethash
                                  generic-function seen-generic-functions t))))
              (emit-generic-function-description
               generic-function stream :reference? reference?)))
          (relevant-generic-functions class))))

(defun emit-word-class-descriptions ()
  (a:with-output-to-file (stream "generated-word-classes.texi"
                                 :if-exists :supersede)
    (emit-word-class-graph stream)
    (let ((seen (make-hash-table :test #'eq)))
      (mapc (a:rcurry #'emit-word-class-description stream seen)
            (all-word-classes)))))

(emit-word-class-descriptions)
