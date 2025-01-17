(cl:in-package #:spell.test)

(fiveam:def-suite* :spell.english
  :in :spell)

(test english.non-existing-word
  "Test dictionary lookup with strings that are not existing words."
  (is (null (spell:english-lookup "no-such-word")))
  ;; A prefix of an existing word exercises a different code path.
  (is (null (spell:english-lookup "fuc")))
  ;; A an existing word with an non-existing suffix exercises yet
  ;; another code path.
  (spell:english-lookup "test'sxxx"))

(test english.compare
  "Compare dictionary data structure to ground truth from dictionary file."
  (let ((fiveam:*test-dribble* nil)
        (*print-right-margin*  80)
        (progress              (progress-reporter)))
    (map-dictionary-entries
     (lambda (word type base &rest initargs)
       (funcall progress)
       (let* ((class-info     (spell::find-word-class type))
              (expected-class (fourth class-info))
              (results        (a:ensure-list (spell:english-lookup word))))
         (fiveam:is-true
          (some (lambda (result)
                  (result-matches-p result expected-class base initargs))
                results)
          "~@<For ~S ~S [base ~S]~@[ initargs ~{~S~^ ~}~], none of the ~
           results ~{~A~^, ~} matches the expected properties.~@:>"
          type word base initargs results))))))
