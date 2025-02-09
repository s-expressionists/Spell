(cl:in-package #:spell)

(defword noun ()
  ((%number :initarg :number
            :type    (member :any :singular :plural)
            :reader  number)
   (%case   :initarg :case
            :type    (member nil :nominative :genitive)
            :reader  case)
   (%gender :initarg :gender
            :type    (member nil :any :masculine :feminine :neuter)
            :reader  gender))
  (:documentation
   "Direct instances represent generic nouns. Also a superclass for more
specific noun classes."))

(defword proper-noun (noun)
  ()
  (:documentation
   "Instances represent proper nouns, that is generally names."))

(defword verb ()
  ((%person      :initarg :person
                 :type    (member :any :first :second :third)
                 :reader  person)
   (%number      :initarg :number
                 :type    (member :any :singular :plural)
                 :reader  number)
   (%tense       :initarg :tense
                 :type    (member nil
                                  :present-simple
                                  :progressive
                                  :past
                                  :perfect-participle
                                  :passive-perfect-participle)
                 :reader  tense)
   (%negative    :initarg :negative
                 :type    boolean
                 :reader  negative)
   (%contraction :initarg :contraction
                 :type    boolean
                 :reader  contraction)
   (%strength    :initarg :strength
                 :type    (member :weak :strong)
                 :reader  strength)
   (%infinitive  :initarg :infinitive
                 :type    (member nil :self :to)
                 :reader  infinitive))
  (:documentation
   "Direct instances represent generic verbs. Also a superclass for more
specific verb classes."))

(defword preposition ()
  ()
  (:documentation
   "Instances represent prepositions, that is words which indicate the
spatial, temporal, logical, etc. relations between other words."))

(defword adjective ()
  ((%degree :initarg :degree
            :type    (member :positive :comparative :superlative)
            :reader  degree))
  (:documentation
   "Instances represent adjectives, that is a property of a noun together
with the degree of that property."))

(defword adverb ()
  ()
  (:documentation
   "Instances represent adverb, that is a property of a verb."))

(defword pronoun ()
  ((%person   :initarg :person
              :type    (member nil :first :second :third)
              :reader  person)
   (%number   :initarg :number
              :type    (member :any :singular :plural)
              :reader  number)
   (%gender   :initarg :gender
              :type    (member nil :masculine :feminine :neuter)
              :reader  gender)
   (%case     :initarg :case
              :type    (member :nominative
                               :genitive
                               :accusative
                               :nominative-or-accusative)
              :reader  case)
   (%negative :initarg :negative
              :type    boolean
              :reader  negative))
  (:documentation
   "Direct instances represent generic pronouns. Also a superclass for
several more specific pronoun classes."))

(defword personal-pronoun (pronoun)
  ()
  (:documentation
   "Instances represent personal pronouns, that is shorthand references to
a noun which designates a person."))

(defword possessive-pronoun (pronoun)
  ((%refnumber :initarg :refnumber
               :type    (member :any :singular :plural)
               :reader  refnumber))
  (:documentation
   "Instances represent possessive pronouns, that is shorthand references
to an object that is possessed by an entity which is identifiable from
the context."))

(defword reflexive-pronoun (pronoun)
  ()
  (:documentation
   "Instances represent possessive pronouns, that is shorthand references
that link the object back to the subject which is identifiable from
the context."))

(defword demonstrative-pronoun (pronoun)
  ()
  (:documentation
   "Instances represent demonstrative pronouns, that is shorthand
references which highlight an entity that is identifiable from the
context."))

(defword interjection ()
  ()
  (:documentation
   "Instances represent interjections, that is words which interrupt the
sequence of words within a sentence or the sequence of sentences."))

(defword conjunction ()
  ()
  (:documentation
   "Direct instances represent conjunctions, that is words which connect
sentences or clauses."))

(defword subordinate (conjunction)
  ()
  (:documentation
   "Instances represent subordinates, that is conjunctions which connect a
dependent and an independent clause."))

(defword determiner ()
  ((%number :initarg :number
            :type    (member :any :singular :plural)
            :reader  number))
  (:documentation
   "Instances are determiners, that is words which determine one or more
aspect, such as the grammatical number, of a noun.  Also a superclass
for several more specific determiner classes."))

(defword article (determiner)
  ((%number      :type    (member :singular :plural))
   (%determinate :initarg :determinate
                 :type    boolean
                 :reader  determinate))
  (:documentation
   "Instances are articles, that is determiners which specify whether a
noun refers to a particular or an arbitrary entity."))

(defword quantifier (determiner)
  ()
  (:documentation
   "Instances are quantifiers, that is determiners which specify count or
amount of a noun."))

(defword possessive-adjective (determiner)
  ((%gender    :initarg :gender
               :type    (member :any :masculine :feminine)
               :reader  gender)
   (%person    :initarg :person
               :type    (member :first :second :third)
               :reader  person)
   (%refnumber :initarg :refnumber
               :type    (member :any :singular :plural)
               :reader  refnumber))
  (:documentation
   "Instances represent possessive adjectives, that is determiners
which mark a noun as being possessed by another entity."))

(defword demonstrative-adjective (determiner)
  ()
  (:documentation
   "Instances represent demonstrative adjectives, that is determiners
which highlight the fact that an associated noun is identifiable from
the context."))

(defword interrogative-adjective (determiner)
  ()
  (:documentation
   "Instances represent interrogative adjectives, that is adjectives "))

(defword noun-verb-contraction (noun verb)
  ()
  (:documentation
   "Instances are of type noun and also of type verb and are formed as a
contraction of one noun and one verb."))

(defword verb-verb-contraction (verb)
  ()
  (:documentation
   "Instances are verbs that are formed as a contraction of two verbs."))
