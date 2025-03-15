(cl:in-package #:spell)

;;; Dictionary protocol

(defgeneric entry-count (dictionary))

(defgeneric map-entries (function dictionary))

(defgeneric lookup (string dictionary))

(defgeneric insert (object string dictionary))

(defgeneric load-dictionary (source &key into))

;;; Trie node protocols

;;; Lookup protocol

(defgeneric node-lookup (function string suffix node))

(defgeneric map-node-entries (function node characters))

;;; Insert protocol

(defgeneric node-insert (object string suffix node))

;;; Leaf node protocol

(defgeneric map-leaf-entries (function node entries))

(defgeneric add-leaf-entry (entry node entries))

;;; Interior node protocol

(defgeneric map-children (function node children))

(defgeneric find-child (string suffix node children))

(defgeneric add-child (char child node children))

;;; Word protocol

(defgeneric base (word)
  (:documentation
   "Return the base word of WORD.

For words of type noun, the base word is usually the nominative,
singular variant of the word.

For words of type verb, the base word is usually the infinitive
variant of the word."))

;;; Noun protocol

(defgeneric number (word)
  (:documentation
   "Return a keyword which indicates the number for entities related to WORD.

If WORD is a verb, the keyword indicates the number of agents who
perform the action.

If WORD is a noun, the keyword indicates the number of objects or
people the noun refers to. Similarly for pronouns.

If WORD is a determiner (or a more specific word sub-type), the number
refers to the associated noun.

Possible values are `:any', `:singular' and `:plural'."))

(defgeneric case (word)
  (:documentation
   "Return a keyword which indicates the grammatical case of WORD.

Possible values are `:nominative', `:genitive', `:accusative' and
`:nominative-or-accusative'."))

(defgeneric gender (word)
  (:documentation
   "Return a keyword which indicates the gender of entities related to WORD.

Possible values are `nil', `:masculine', `:feminine' and `:neuter'."))

;;; Verb protocol

(defgeneric person (word)
  (:documentation
   "Return a keyword which indicates the grammatical person of entities
associated with WORD.

Possible values are `:any', `:first', `:second', `:third'."))

(defgeneric tense (word)
  (:documentation
   "Return a keyword which indicates the time of the action for WORD.

Possible values are `nil', `:present-simple', `:progressive', `:past',
`:perfect-participle', `:passive-perfect-participle'."))

(defgeneric negative (word)
  (:documentation
   "Return a Boolean which indicates whether the meaning of WORD is in
some way negative.

If WORD is a verb, a true value indicates that the absence of the
action represented by the verb.

If WORD is a pronoun, a true value indicates the complement of the set
of entities for which the non-negated pronoun would stand in."))

(defgeneric contraction (word)
  (:documentation
   "Return a Boolean which indicates whether WORD is a contraction of two
words.

For instances of the `verb' class a true value of the contraction
property always goes together with a true value of the negative
property.  In other words these contractions consist of the base verb
and the word \"not\" as in \"won't\"."))

(defgeneric strength (word)
  (:documentation
   "Return a keyword which indicates the grammatical strength of WORD.

Only `verb' instances have the strength property which is either weak
or strong.  Weak means that the past tense of the verb is formed by
adding a suffix while strong means that the past tense form of the
verb has a different stem than the infinitive form.

Possible values are `:weak' and `:strong'."))

(defgeneric infinitive (word)
  (:documentation
   "Return a keyword which indicates whether and which kind of infinitive
WORD is.

Possible values are `nil', `:self' and `:to'."))

;;; Adjective protocol

(defgeneric degree (word)
  (:documentation
   "Return a keyword which indicates the degree for WORD.

Possible values are `:positive', `:comparative' and `:superlative'."))

;;; Pronoun protocol

;;; Possessive pronoun protocol

(defgeneric refnumber (word)
  (:documentation
   "Return a keyword which indicates the grammatical number of the
possessing entity referenced by WORD.

Possible values are `:any', `:singular' and `:plural'."))

;;; Article protocol

(defgeneric determinate (word)
  (:documentation
   "Return a Boolean which indicates whether the noun associated with WORD
refers to particular or an arbitrary entity."))
