(cl:in-package #:spell.test)

(defun progress-reporter (&key (stream *trace-output*) (indicator #\.))
  (let ((i 0))
    (lambda ()
      (multiple-value-bind (j k) (floor i 1000)
        (when (zerop k)
          (when (and *print-right-margin*
                     (zerop (mod j *print-right-margin*)))
            (format stream "~&;; "))
          (write-char indicator stream)
          (force-output stream)))
      (incf i))))

(defun map-dictionary-entries (function
                               &key (files '("data/english.txt"
                                             "data/english-additions.txt")))
  (mapc (lambda (file)
          (with-open-file (stream (asdf:system-relative-pathname
                                   "spell/test" file))
            (spell::map-dictionary-file-entries function stream)))
        files))

(defun result-matches-p (result class base initargs)
  (and (typep result class)
       (string= (spell:base result) base)
       (loop :for (key value) :on initargs :by #'cddr
             :for function = (find-symbol (symbol-name key)
                                          (find-package '#:spell))
             :always (equal (funcall function result) value))))
