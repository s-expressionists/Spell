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

(defmethod map-entries ((function t) (dictionary dictionary))
  (let ((function   (a:ensure-function function))
        ;; Since trie nodes do not contain the actual spellings of the
        ;; respective associated words, we keep track of the path to
        ;; the currently visited node in this array by pushing and
        ;; popping characters.
        (characters (make-array 20 :element-type 'character
                                   :adjustable   t
                                   :fill-pointer 0)))
    (flet ((visit (entry characters)
             (let* (;; We copy CHARACTERS since we don't want the base
                    ;; of WORD to change retroactively.
                    (spelling (copy-seq characters))
                    (word     (expand-entry entry spelling)))
               (funcall function spelling word))))
      (declare (dynamic-extent #'visit))
      (map-node-entries #'visit (contents dictionary) characters))))

(defmethod lookup ((string string) (dictionary dictionary))
  (let ((length (length string))
        (result '()))
    (node-lookup (lambda (word)
                   (push word result))
                 string length (contents dictionary))
    result))

(defmethod insert ((object t) (string string) (dictionary dictionary))
  (let ((root (contents dictionary)))
    (when (typep root 'compact-node)
      (error "~@<Cannot insert into compacted dictionary.~@:>"))
    (node-insert object string (length string) root)))

(defmethod compact ((dictionary dictionary))
  (setf (contents dictionary) (with-string-interning ()
                                (compact-node (contents dictionary) 0)))
  dictionary)

(defmethod share-structure ((dictionary dictionary) &rest args &key depth-limit)
  (declare (ignore depth-limit))
  (setf (contents dictionary) (apply #'share-structure (contents dictionary)
                                     args))
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
