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

(defun set-equal/equal (a b)
  (a:set-equal a b :test #'equal))

(defun set-equal/string (a b)
  (a:set-equal a b :test #'string=))

(defun entry-equal (entry1 entry2)
  (destructuring-bind (spelling1 word1 distance1) entry1
    (destructuring-bind (spelling2 word2 distance2) entry2
      (and (string= spelling1 spelling2)
           (eq (class-of word1) (class-of word2))
           (loop :for slot :in (spell::bitfield-slots (class-of word1))
                 :for name  =   (c2mop:slot-definition-name slot)
                 :always (eql (slot-value word1 name)
                              (slot-value word2 name)))
           (eql distance1 distance2)))))

(defun set-equal/entry (set1 set2)
  (a:set-equal set1 set2 :test #'entry-equal))
