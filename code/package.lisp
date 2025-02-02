(cl:defpackage #:spell
  (:use
   #:cl)

  (:local-nicknames
   (#:a #:alexandria))

  (:shadow
   #:case
   #:number)

  ;; Word protocol
  (:export
   #:base)

  ;; Noun protocol
  (:export
   #:number
   #:case
   #:gender)

  ;; Verb protocol
  (:export
   #:person
   #:number
   #:tense
   #:mood
   #:negative
   #:contraction
   #:strength
   #:infinitive)

  ;; Adjective protocol
  (:export
   #:degree)

  ;; Pronoun protocol
  (:export
   #:person
   #:number
   #:gender
   #:case)

  ;; Possessive pronoun protocol
  (:export
   #:refnumber)

  ;; Determiner protocol
  (:export
   #:number)

  ;; Article protocol
  (:export
   #:number
   #:determinate)

  ;; Possessive adjective protocol
  (:export
   #:gender
   #:person
   #:refnumber)

  ;; Dictionary protocol
  (:export
   #:entry-count
   #:map-entries
   #:lookup
   #:map-similar
   #:insert
   #:load-dictionary)

  (:export
   #:english-lookup
   #:english-check-paragraph))
