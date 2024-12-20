(in-package #:spell)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun load-english-dictionary (&key (verbose *compile-verbose*))
    (let ((filename   (asdf:system-relative-pathname
                       "spell" "data/english.txt"))
          dictionary entry-count)
      (flet ((note (format-control &rest format-arguments)
               (when verbose
                 (let ((stream *standard-output*))
                   (fresh-line stream)
                   (pprint-logical-block (stream nil :per-line-prefix "; ")
                     (apply #'format stream format-control format-arguments))
                   (force-output stream)))))
        (note "Loading dictionary ~:_~S" filename)
        (setf (values dictionary entry-count) (load-dictionary filename))
        (note "Will dump dictionary with ~:_~:D entr~:@P" entry-count))
      dictionary)))

(defparameter *english-dictionary* #.(load-english-dictionary))

(defun english-lookup (word)
  (when (and word (string/= word ""))
    (let ((dictionary *english-dictionary*))
      (flet ((try (variant)
               (let ((result (lookup variant dictionary)))
                 (when result
                   (return-from english-lookup result)))))
        (try word)
        (let* ((initial   (aref word 0))
               (downcased (char-downcase initial)))
          (unless (char= initial downcased)
            ;; We change, for example, "Anti-Semitic" at the beginning
            ;; of a sentence to "anti-Semitic" which is in the
            ;; dictionary.
            (let ((decapitalized (copy-seq word)))
              (setf (aref decapitalized 0) downcased)
              (try decapitalized))
            ;; We change, for example, "PARAMETER" (which is typical
            ;; for some commenting styles) to "parameter" which is in
            ;; the dictionary.
            (when (every #'upper-case-p word)
              (try (string-downcase word)))))))))

(declaim (inline english-text-char-p find-start find-end))
(defun english-text-char-p (character)
  (declare (character character))
  (or (char= character #\')
      (alpha-char-p character)))

(defun find-start (string position)
  (loop with length = (length string)
        for i from position
        when (or (= i length)
                 (english-text-char-p (char string i)))
          return i))

(defun find-end (string position)
  (loop with length = (length string)
        for i from position
        when (or (= i length)
                 (not (english-text-char-p (char string i))))
          return i))

(defun english-check-paragraph (string)
  ;; TODO optimize, mayhaps. We do not need to create subsequences of the
  ;; paragraph, we can traverse the dictionary using offsets of that paragraph.
  (loop with position = 0
        with length = (length string)
        for word-start = (find-start string position)
        for word-end = (find-end string word-start)
        until (= word-start word-end length)
        do (setf position word-end)
        unless (english-lookup (subseq string word-start word-end))
          collect (cons word-start word-end)))
