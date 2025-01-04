(cl:in-package #:spell)

;;; Dictionary

(defclass dictionary (utilities.print-items:print-items-mixin)
  ((%contents    :accessor contents
                 :initform (make-instance 'raw-node))
   (%entry-count :accessor entry-count
                 :initform 0)))

(defmethod make-load-form ((object dictionary) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod utilities.print-items:print-items append ((object dictionary))
  `((:entry-count "~:D entr~:@P" ,(entry-count object))))

(defmethod lookup ((string string) (dictionary dictionary))
  (let ((length (length string))
        (result '()))
    (check-type length (integer 1) "Query string must not be empty")
    (%lookup (lambda (word)
               (push word result))
             string length (contents dictionary))
    result))

(defmethod insert ((object t) (string string) (dictionary dictionary))
  (let ((root (contents dictionary)))
    (when (typep root 'compact-node)
      (error "~@<Cannot insert into compacted dictionary.~@:>"))
    (%insert object string (length string) root)))

(defmethod compact ((dictionary dictionary))
  (setf (contents dictionary) (with-string-interning ()
                                (compact-node (contents dictionary) 0)))
  dictionary)

;;; Dictionary loading

(defmethod load-dictionary ((source stream)
                            &key (into (make-instance 'dictionary)))
  (let ((count 0))
    (with-string-interning ()
      (map-dictionary-file-entries
       (lambda (spelling type base &rest initargs)
         (let* ((spelling (intern-string spelling))
                (base     (intern-string base))
                (word     (apply #'make-word spelling type base initargs)))
           (insert word spelling into))
         (incf count))
       source))
    (incf (entry-count into) count)
    into))

(defmethod load-dictionary ((source pathname) &rest args &key into)
  (declare (ignore into))
  (with-open-file (stream source)
    (apply #'load-dictionary stream args)))

(defmethod load-dictionary ((source string) &rest args &key into)
  (declare (ignore into))
  (apply #'load-dictionary (pathname source) args))

(defmethod load-dictionary ((source sequence)
                            &key (into (make-instance 'dictionary)))
  (mapc (lambda (source)
          (with-simple-restart (skip "Skip source ~A" source)
            (apply #'load-dictionary source :into into)))
        source))
