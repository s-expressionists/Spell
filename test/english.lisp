(cl:in-package #:spell.test)

(fiveam:def-suite* :spell.compare
  :in :spell)

(defun map-dictionary-entries (function)
  (with-open-file (stream (asdf:system-relative-pathname
                           "spell/test" "data/english.txt"))
    (spell::map-dictionary-file-entries function stream)))

(defun progress-reporter (&key (stream *trace-output*) (indicator #\.))
  (let ((i 0))
    (lambda ()
      (multiple-value-bind (j k) (floor i 1000)
        (when (zerop k)
          (when (and *print-right-margin*
                     (zerop (mod j *print-right-margin*)))
            (format stream "~&; "))
          (write-char indicator stream)
          (force-output stream)))
      (incf i))))

(test compare.english
  "Compare dictionary data structure to ground truth from dictionary file."
  (let ((fiveam:*test-dribble* nil)
        (*print-right-margin*  80)
        (progress              (progress-reporter)))
    (map-dictionary-entries
     (lambda (word type base &rest initargs)
       (funcall progress)
       (let* ((expected-class (gethash type spell::*word-types*))
              (results        (a:ensure-list (spell:english-lookup word))))
         (flet ((result-matches-p (result)
                  (and (typep result expected-class)
                       (string= base (spell:base result))
                       (loop :for (key value) :on initargs :by #'cddr
                             :for function = (find-symbol (symbol-name key)
                                                          (find-package '#:spell))
                             :always (equal (funcall function result) value)))))
           (fiveam:is-true
            (some #'result-matches-p results)
            "~@<For ~S ~S [base ~S]~@[ initiargs ~{~S~^ ~}~], none of the ~
             results ~{~A~^, ~} matches the expected properties.~@:>"
            type word base initargs results)))))))
