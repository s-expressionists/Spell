(cl:in-package #:spell.test)

(fiveam:def-suite* :spell.dictionary
  :in :spell)

(test dictionary.lookup
  "Make sure that `lookup' behaves correctly for an empty dictionary."
  (let ((dictionary (make-instance 'spell::dictionary)))
    (is (equal '() (spell:lookup "" dictionary)))
    (is (equal '() (spell:lookup "no-such-word" dictionary)))))

(test dictionary.insert
  "Make sure that an attempt to insert into a compact dictionary signals
an error."
  (let ((dictionary spell::*english-dictionary*)
        (word       (spell::make-word "some-word" :noun "some-word")))
    (signals error (spell:insert word "word" dictionary))))

(test dictionary.print
  "Make sure that instances of the `dictionary' class can be printed."
  (let ((dictionary (make-instance 'spell::dictionary)))
    (is-true (search "0 entries" (princ-to-string  dictionary))))
  (let ((dictionary spell::*english-dictionary*))
    (is-false (a:emptyp (princ-to-string dictionary)))))
