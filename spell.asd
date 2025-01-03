(defsystem "spell"
  :description "Spellchecking package for Common Lisp"
  :author ("Robert Strandh <robert.strandh@gmail.com>"
           "Michał \"phoe\" Herda <phoe@disroot.org>"
           "Jan Moringen <jan.moringen@posteo.de>")
  :license "BSD"
  :version (:read-file-form "data/version-string.sexp")
  :depends-on ("alexandria")
  :components ((:module     "code"
                :serial     t
                :components ((:file "package")
                             (:file "protocol")
                             (:file "text-file")
                             (:file "word")
                             (:file "spell")
                             (:file "english"))))

  :in-order-to ((test-op (test-op "spell/test"))))

(defsystem "spell/simple"
  :description "Spellchecking package for Common Lisp - simple version"
  :author ("Robert Strandh <robert.strandh@gmail.com>"
           "Michał \"phoe\" Herda <phoe@disroot.org>"
           "Jan Moringen <jan.moringen@posteo.de>")
  :license "BSD"
  :version (:read-file-form "data/version-string.sexp")
  :components ((:module     "code"
                :serial     t
                :components ((:file "package")
                             (:file "protocol")
                             (:file "simple")
                             (:file "spell")
                             (:file "english")))))

(defsystem "spell/test"
  :description "Unit tests for the spell system."
  :depends-on  ("fiveam"

                (:version "spell" (:read-file-form "data/version-string.sexp")))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file "package")
                              (:file "utilities")
                              (:file "english"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:spell.test '#:run-tests)))
