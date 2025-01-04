(cl:defpackage #:spell.test
  (:use
   #:cl)

  (:local-nicknames
   (#:a #:alexandria))

  (:import-from #:fiveam
   #:test
   #:is
   #:is-true
   #:is-false)

  (:export
   #:run-tests))

(cl:in-package #:spell.test)

(fiveam:def-suite :spell)

(defun run-tests ()
  (fiveam:run! :spell))
