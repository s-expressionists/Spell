(cl:in-package #:spell.test)

(fiveam:def-suite* :spell.compare
  :in :spell)

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
         (fiveam:is-true
          (some (lambda (result)
                  (result-matches-p result expected-class base initargs))
                results)
          "~@<For ~S ~S [base ~S]~@[ initiargs ~{~S~^ ~}~], none of the ~
           results ~{~A~^, ~} matches the expected properties.~@:>"
          type word base initargs results))))))
