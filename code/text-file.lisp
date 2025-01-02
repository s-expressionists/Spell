(cl:in-package #:spell)

(defun map-dictionary-file-entries (function stream)
  (labels ((process-line (line)
             (with-input-from-string (stream line)
               (let ((spelling (read stream nil stream))
                     (base     (progn
                                 (assert (eq :base (read stream nil stream)))
                                 (read stream nil stream)))
                     (type     (progn
                                 (assert (eq :type (read stream nil stream)))
                                 (read stream nil stream)))
                     (initargs (loop :for expression = (read stream nil stream)
                                     :until (eq expression stream)
                                     :collect expression)))
                 (apply function spelling type base initargs)))))
    (loop :for line = (read-line stream nil nil)
          :while line
          :when (and (plusp (length line))
                     (not (eql (aref line 0) #\;)))
            :do (process-line line))))
