(cl:in-package #:spell.test)

(fiveam:def-suite* :spell.english
  :in :spell)

(test english.non-existing-word
  "Test dictionary lookup with strings that are not existing words."
  (is (null (spell:english-lookup "no-such-word")))
  ;; A prefix of an existing word exercises a different code path.
  (is (null (spell:english-lookup "fuc"))))

(test english.compare
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
