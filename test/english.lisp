(cl:in-package #:spell.test)

(fiveam:def-suite* :spell.english
  :in :spell)

;;; Tests for `english-lookup'

(test english.non-existing-word
  "Test dictionary lookup with strings that are not existing words."
  (is (null (spell:english-lookup "no-such-word")))
  ;; A prefix of an existing word exercises a different code path.
  (is (null (spell:english-lookup "fuc")))
  ;; A an existing word with an non-existing suffix exercises yet
  ;; another code path.
  (spell:english-lookup "test'sxxx"))

(test english.compare/lookup
  "Compare `lookup' results to ground truth from dictionary file."
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

(test english.compare/map-entries
  "Compare `map-entries' to ground truth from dictionary file."
  (let ((fiveam:*test-dribble* nil)
        (*print-right-margin*  80)
        (table                 (make-hash-table :test #'equal)))
    (format *trace-output* "~&;; Enumerating dictionary items~%")
    (let ((progress (progress-reporter)))
      (spell:map-entries (lambda (spelling word)
                           (declare (ignore word))
                           (funcall progress)
                           (setf (gethash spelling table) nil))
                         spell::*english-dictionary*)
      (format *trace-output* "~&;; ~:D item~:P~%" (1- (funcall progress))))
    (format *trace-output* "~&;; Enumerating ground truth~%")
    (let ((progress (progress-reporter)))
      (map-dictionary-entries
       (lambda (word type base &rest initargs)
         (declare (ignore type base initargs))
         (funcall progress)
         (is-true (nth-value 1 (gethash word table))
                  "~@<Ground truth word ~S is not in the enumerated ~
                   dictionary items.~@:>"
                  word)
         (setf (gethash word table) t)))
      (format *trace-output* "~&;; ~:D item~:P~%" (1- (funcall progress))))
    (let ((extra-words (remove t (a:hash-table-alist table) :key #'cdr)))
      (is (null extra-words)
          "~@<The enumerated dictionary items contain extra words ~
           ~{~S~^, ~}.~@:>"
          extra-words))))

(test contractions.one-component
  "Check that the base word of `verb' with :contraction t is in the
dictionary."
  (let ((dictionary spell::*english-dictionary*))
    (spell:map-entries
     (lambda (spelling word)
       (declare (ignore spelling))
       (when (and (typep word 'spell::verb)
                  (spell:contraction word))
         (let ((base (spell:base word)))
           (is-true (spell:lookup base dictionary)))))
     dictionary)))

(test contractions.two-components
  "Check that the base words of `noun-verb-contraction's and
`verb-verb-contraction's are in the dictionary."
  (let ((dictionary spell::*english-dictionary*))
    (spell:map-entries
     (lambda (spelling word)
       (declare (ignore spelling))
       (when (or (typep word 'spell::noun-verb-contraction)
                 (typep word 'spell::verb-verb-contraction))
         (let* ((base  (spell:base word))
                (index (position #\Space base))
                (word1 (subseq base 0 index))
                (word2 (subseq base (1+ index))))
           (is-true (spell:lookup word1 dictionary))
           (is-true (spell:lookup word2 dictionary)))))
     dictionary)))

;;; Tests for `english-corrections'

(test english-corrections/empty
  "Smoke test for `english-corrections' with the empty word."
  (is (equal (values '() nil) (spell:english-corrections "" :threshold 0)))
  (multiple-value-bind (corrections found?)
      (spell:english-corrections "" :threshold 1)
    (mapc (lambda (suggestion) (is (= 1 (length suggestion)))) corrections)
    (is-false found?)))

(test english-corrections/variants
  "Smoke test for `english-corrections' with just spelling and variants."
  ;; Threshold 0 for invalid and valid word.
  (flet ((one-variant (string)
           (is (equal (values '() nil)
                      (spell:english-corrections string :threshold 0)))))
    (one-variant "circumstanzes")
    (one-variant "Circumstanzes")
    (one-variant "CIRCUMSTANZES"))
  (flet ((one-variant (string)
           (is (equal (values '("circumstances") t)
                      (spell:english-corrections string :threshold 0)))))
    (one-variant "circumstances")
    (one-variant "Circumstances")
    (one-variant "CIRCUMSTANCES"))
  ;; Threshold 1 for invalid and valid word.
  (flet ((one-variant (string)
           (multiple-value-bind (corrections found?)
               (spell:english-corrections string :threshold 1)
             (is (set-equal/string '("circumstances") corrections))
             (is-false found?))))
    (one-variant "circumstanzes")
    (one-variant "Circumstanzes")
    (one-variant "CIRCUMSTANZES"))
  (flet ((one-variant (string)
           (multiple-value-bind (corrections found?)
               (spell:english-corrections string :threshold 1)
             (is (set-equal/string '("circumstances" "circumstance"
                                     "circumstances'" "circumstance's")
                                   corrections))
             (is-true found?))))
    (one-variant "circumstances")
    (one-variant "Circumstances")
    (one-variant "CIRCUMSTANCES")))

(test english-corrections/word
  "Smoke test for `english-corrections' with spelling and word."
  (let* ((results (spell:english-corrections "circumstanzes" :threshold 1
                                                             :group-by  :entry))
         (result  (first results)))
    (is (= 1 (length results)))
    (destructuring-bind (word . spelling) result
      (is (string= "circumstances" spelling))
      (is-true (typep word 'spell::noun))
      (is (string= "circumstance" (spell:base   word)))
      (is (eql     :plural        (spell:number word)))
      (is (eql     nil            (spell:case   word)))
      (is (eql     nil            (spell:gender word))))))

(test english-corrections/threshold-2
  "Smoke test for `english-corrections' with threshold 2."
  (is (set-equal/string
       #1='("circumstances" "circumstance" "circumstances'" "circumstance's")
       (spell:english-corrections "circumstanzes" :threshold 2)))
  ;; Requesting fewer suggestions must result in a subset of the
  ;; requested size.
  (let* ((results      (spell:english-corrections "circumstanzes" :threshold 2
                                                                  :count     2))
         (intersection (intersection #1# results :test #'string=)))
    (is (= 2 (length results)))
    (is (set-equal/string results intersection))))
