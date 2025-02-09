(cl:defpackage #:spell.tools.write-news
  (:use
   #:cl)

  (:local-nicknames
   (#:rc #:spell.tools.read-changes))

  (:export
   #:write-news))

(cl:in-package #:spell.tools.write-news)

(defmacro define-emitter (name (keyword &rest lambda-list) &body body)
  `(defun ,name (node stream format)
     (destructuring-bind (keyword ,@lambda-list) node
       (assert (eq keyword ,keyword))
       ,@body)))

(define-emitter write-paragraph (:paragraph &rest content)
  (pprint-logical-block (stream content)
    (loop :for (chunk next) :on content
          :do (etypecase chunk
                ((cons (eql :when)))
                ((cons (eql :symbol))
                 (if (eq format :markdown)
                     (format stream "`~A`" (second chunk))
                     (format stream "~:@(~A~)" (second chunk))))
                ((cons (eql :tt))
                 (if (eq format :markdown)
                     (format stream "`~A`" (second chunk))
                     (write-string (second chunk) stream)))
                (string
                 (write-string chunk stream)))
          :when (and next
                     (not (or (eq (rc:punctuationp next) t)
                              (typep next '(cons (eql :when)))))
                     (not (eq (rc:punctuationp chunk) :open)))
            :do (write-string " " stream)
                (pprint-newline :fill stream))))

(define-emitter write-code (:code language content)
  (when (eq format :markdown)
    (format stream "```~@[~A~]~@:_" (ecase language
                                      ((nil) nil)
                                      (:common-lisp "cl"))))
  (pprint-logical-block
      (stream node :per-line-prefix (if (eq format :plaintext)
                                        "  "
                                        ""))
    (let ((lines (rc:split-into-lines content)))
      (loop :for (line next) :on lines
            :do (write-string line stream)
            :when next
              :do (pprint-newline :mandatory stream))))
  (when (eq format :markdown)
    (format stream "~@:_```")))

(define-emitter write-item (:item &rest children)
  (format stream "* ")
  (pprint-logical-block (stream children)
    (loop :for (child next) :on children
          :do (etypecase child
               ((cons (eql :paragraph))
                (write-paragraph child stream format))
               ((cons (eql :code))
                (write-code child stream format)))
          :when next
            :do (format stream "~@:_~@:_")))
  (format stream "~@:_~@:_"))

(define-emitter write-release (:release version date &rest items)
  (when (eq format :markdown)
    (format stream "# "))
  (format stream "Release ~A (~:[not yet released~;~:*~A~])~@:_~
                  ~@:_"
          version date)
  (dolist (item items)
    (write-item item stream format)))

(defun emit-news (changes format &key (stream *standard-output*) count)
  (let ((*print-right-margin* (if (eq format :markdown)
                                  most-positive-fixnum
                                  nil)))
    (pprint-logical-block (stream changes)
      (destructuring-bind (keyword &rest releases) changes
        (assert (eq keyword :changes))
        (loop :repeat (or count (length releases))
              :for release :in releases
              :do (write-release release stream format))))))

(destructuring-bind (changes-file &optional (format "plaintext"))
    (rest sb-ext:*posix-argv*)
  (let* ((format  (find-symbol (string-upcase format) '#:keyword))
         (changes (rc:read-changes changes-file)))
    (emit-news changes format)))
