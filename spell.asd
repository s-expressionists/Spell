(defsystem "spell"
  :description "Spellchecking package for Common Lisp"
  :author ("Robert Strandh <robert.strandh@gmail.com>"
           "Michał \"phoe\" Herda <phoe@disroot.org>"
           "Jan Moringen <jan.moringen@posteo.de>")
  :license "BSD"
  :version (:read-file-form "data/version-string.sexp")
  :components ((:module     "code"
                :serial     t
                :components ((:file "package")
                             (:file "protocol")
                             (:file "word")
                             (:file "spell")
                             (:file "english")))))

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
