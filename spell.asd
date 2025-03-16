(defsystem "spell"
  :description "Spellchecking package for Common Lisp"
  :license     "BSD" ; see LICENSE and english.LICENSE files for details
  :author      ("Robert Strandh <robert.strandh@gmail.com>"
                "Michał \"phoe\" Herda <phoe@disroot.org>"
                "Jan Moringen <jan.moringen@posteo.de>")

  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  ("closer-mop"
                "alexandria"
                "utilities.print-items"
                "bitfield")

  :components  ((:module     "code"
                 :serial     t
                 :components ((:file "package")
                              (:file "protocol")
                              ;; Utilities
                              (:file "bitfield-class")
                              (:file "strings")
                              (:file "text-file")
                              ;; Words
                              (:file "word-class") ; metaclass
                              (:file "word-classes")
                              ;; Trie
                              (:file "trie")
                              (:file "raw-trie")
                              (:file "compact-trie")
                              (:file "shared-trie")
                              ;; Dictionary
                              (:file "dictionary")
                              ;; Similar
                              (:file "similar")))

                (:module     "english-dictionary-data"
                 :pathname   "data"
                 :components ((:static-file "english.txt")
                              (:static-file "english-additions.txt")))

                (:module     "english-dictionary"
                 :pathname   "code"
                 :depends-on ("code"
                              "english-dictionary-data")
                 :components ((:file "english"))))

  :in-order-to ((test-op (test-op "spell/test"))))

(defsystem "spell/simple"
  :description "Identical to \"spell\". Exists for backward compatibility."
  :version     (:read-file-form "data/version-string.sexp")
  :depends-on  ((:version "spell" (:read-file-form "data/version-string.sexp"))))

(defsystem "spell/test"
  :description "Unit tests for the spell system."
  :depends-on  ("fiveam"

                (:version "spell" (:read-file-form "data/version-string.sexp")))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file "package")
                              (:file "utilities")
                              (:file "similar")
                              (:file "dictionary")
                              (:file "english"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:spell.test '#:run-tests)))
