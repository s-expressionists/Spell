(cl:in-package #:spell.test)

(fiveam:def-suite* :spell.word
  :in :spell)

(test word.print
  "Make sure that instances of the `word' class can be printed."
  (let ((fiveam:*test-dribble* nil)
        (*print-right-margin*  80)
        (progress              (progress-reporter)))
    (map-dictionary-entries
     (lambda (word type base &rest initargs)
       (funcall progress)
       (let* ((class  (gethash type spell::*word-types*))
              (result (find-if
                       (lambda (result)
                         (result-matches-p result class base initargs))
                       (a:ensure-list (spell:english-lookup word)))))
         (is-false (null result))
         (let ((string (princ-to-string result)))
           (is-false (a:emptyp string))
           (is (search (string type) string))
           (is (search base          string))))))))
