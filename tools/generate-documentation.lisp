(cl:defpackage #:spell.tools.generate-documentation
  (:use
   #:cl)

  (:local-nicknames
   (#:ti #:spell.tools.texinfo)))

(cl:in-package #:spell.tools.generate-documentation)

;;; Utilities

(defun emit-reference (namespace name stream)
  (let* ((type   (ecase namespace
                   (:class            "Class")
                   (:generic-function "Generic-Function")))
         (name*  (symbol-name name))
         (target (format nil "~A ~(~A~)|~(~A~)"
                         type (package-name (symbol-package name)) name*)))
    (ti:write-reference target (string-downcase name*) stream)))

(defun emit-documentation (documentation stream)
  (flet ((word (start end previous)
           (when start
             (when (member previous '(:space :newline :space-punct))
               (write-char #\Space stream)
               (pprint-newline :fill stream))
             (let ((word (subseq documentation start end)))
               (cond ((eq previous :symbol)
                      (ti:write-code word stream))
                     ((every #'upper-case-p word)
                      (ti:write-var (string-downcase word) stream))
                     (t
                      (ti:write-escaped word stream)))))))
    (loop :with end      = (length documentation)
          :with start    = nil
          :with previous = nil
          :for  i        :below end
          :for  char     =      (aref documentation i)
          :do (cond ((eql char #\`)
                     (word start i previous)
                     (when (or start (member previous '(:space :newline)))
                       (write-char #\Space stream))
                     (setf start (1+ i) previous :symbol))
                    ((eql char #\')
                     (word start i previous)
                     (setf start nil previous :space-punct))
                    ((eq previous :symbol))
                    ((alpha-char-p char)
                     (when (null start)
                       (setf start i)))
                    ((eql char #\()
                     (word start i previous)
                     (when (or start (member previous '(:space :newline)))
                       (write-char #\Space stream))
                     (write-char char stream)
                     (setf start nil previous :punct))
                    ((member char '(#\. #\, #\; #\: #\! #\? #\)))
                     (word start i previous)
                     (write-char char stream)
                     (setf start nil previous :space-punct))
                    ((eql char #\Newline)
                     (word start i previous)
                     (setf start nil)
                     (cond ((eq previous :many-newlines))
                           ((eq previous :newline)
                            (format stream "~@:_~@:_")
                            (setf previous :many-newlines))
                           (t
                            (setf previous :newline))))
                    (t ; space
                     (word start i previous)
                     (setf previous :space start nil)))
          :finally (word start end previous))))

;;; Word class descriptions

(defun relevant-direct-subclasses (class)
  (flet ((badp (subclass)
           (or (subtypep subclass 'spell::implicit-base-mixin)
               (subtypep subclass 'spell::explicit-base-mixin)
               (some (lambda (superclass)
                       (and (not (eq superclass class))
                            (subtypep superclass class)))
                     (sb-mop:class-direct-superclasses subclass)))))
    (let* ((all         (sb-mop:class-direct-subclasses class))
           (interesting (remove-if #'badp all)))
      (sort interesting #'string< :key #'class-name))))

(defun map-word-classes (function)
  (let ((seen '()))
    (labels ((visit (class)
               (unless (eq seen (pushnew class seen :test #'eq))
                 (funcall function class)
                 (mapc #'visit (relevant-direct-subclasses class)))))
      (visit (find-class 'spell::word)))))

(defmethod relevant-generic-functions ((class t))
  (sb-mop:specializer-direct-generic-functions class))

(defmethod relevant-generic-functions ((class (eql (find-class 'spell::word))))
  (list #'spell:base))

(defun emit-word-class-graph (stream)
  (ti:code (stream)
    (let ((utilities.print-tree:*use-unicode?* nil))
      (utilities.print-tree:print-tree
       stream (find-class 'spell::word)
       (utilities.print-tree:make-node-printer
        (lambda (stream depth node)
          (declare (ignore depth))
          (emit-reference :class (class-name node) stream))
        nil
        #'relevant-direct-subclasses)))))

(defun emit-generic-function-description (generic-function stream
                                          &key reference?)
  (let ((name (sb-mop:generic-function-name generic-function)))
    (if reference?
        (progn
          (format stream "~2%The generic function ")
          (emit-reference :generic-function name stream)
          (format stream " can be applied to words of this class.~@:_"))
        (let ((documentation (documentation generic-function t)))
          (format stream "~2%@defgena{~(~A~),~(~A~)} ~{~(~A~)~^ ~}~@:_"
                  (symbol-name name) (package-name (symbol-package name))
                  (sb-mop:generic-function-lambda-list generic-function))
          (if documentation
              (emit-documentation documentation stream)
              (ti:write-emph "not documented" stream))
          (format stream "~@:_@end deffn~@:_")))))

(defun emit-word-class-description
    (class example-words stream seen-generic-functions)
  (let* ((superclasses     (sb-mop:class-direct-superclasses class))
         (name             (class-name class))
         (superclass-names (mapcar #'class-name superclasses))
         (documentation    (documentation class t)))
    (ti:write-section (format nil "Word Class ~(~A~)" name) stream
                      :kind :subheading)
    (format stream "@defclassa{~(~A~),~(~A~)} ("
            (symbol-name name) (package-name (symbol-package name)))
    (loop :for superclass :in (set-difference
                               superclass-names
                               '(utilities.print-items:print-items-mixin))
          :for first? = t :then nil
          :unless first?
            :do (format stream ", ")
          :do (emit-reference :class superclass stream))
    (format stream ")~@:_")
    (if documentation
        (emit-documentation documentation stream)
        (ti:write-emph "not documented" stream))
    (format stream "~@:_@end deftp~@:_")

    (mapc (lambda (generic-function)
            (let ((seen? (nth-value
                          1 (gethash generic-function seen-generic-functions))))
              (unless seen?
                (setf (gethash generic-function seen-generic-functions) t))
              (emit-generic-function-description
               generic-function stream :reference? seen?)))
          (relevant-generic-functions class))

    (unless (null example-words)
      (format stream "~2%Examples~@
                      @multitable @columnfractions .2 .7~@:_")
      (loop :repeat 3
            :for (spelling . word) :in example-words
            :do (ti:item (stream)
                  (lambda (stream)
                    (format stream "~A @tab " spelling)
                    (ti:write-code (princ-to-string word) stream))))
      (format stream "@end multitable~%"))))

(defun example-words ()
  (let ((result (make-hash-table :test #'eq))
        (i      0))
    (spell:map-entries
     (lambda (spelling word)
       (let* ((class    (second (sb-mop:class-direct-superclasses
                                 (class-of word))))
              (existing (gethash class result '())))
         (when (or (< (length existing) 3)
                   (and (not (find #\' spelling))
                        (> (length spelling) 5)
                        (or (< (length existing) 10)
                            (zerop (mod (incf i) 377)))))
           (setf (gethash class result '())
                 (list* (cons spelling word) existing)))))
     spell::*english-dictionary*)
    result))

(defun emit-word-class-descriptions (stream)
  (pprint-logical-block (stream nil)
    (emit-word-class-graph stream)
    (let ((example-words (example-words))
          (seen          (make-hash-table :test #'eq)))
      (map-word-classes
       (lambda (word-class)
         (let ((example-words (gethash word-class example-words)))
           (emit-word-class-description
            word-class example-words stream seen)))))))

(emit-word-class-descriptions *standard-output*)
