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
            :reader  gender)))

(defword proper-noun (noun) ())

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
                 :reader  infinitive)))

(defword preposition () ())

(defword adjective ()
  ((%degree :initarg :degree
            :type    (member :positive :comparative :superlative)
            :reader  degree)))

(defword adverb () ())

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
              :reader  negative)))

(defword personal-pronoun (pronoun) ())

(defword possessive-pronoun (pronoun)
  ((%refnumber :initarg :refnumber
               :type    (member :any :singular :plural)
               :reader  refnumber)))

(defword reflexive-pronoun (pronoun) ())

(defword demonstrative-pronoun (pronoun) ())

(defword interjection () ())

(defword conjunction () ())

(defword subordinate (conjunction) ())

(defword determiner ()
  ((%number :initarg :number
            :type    (member :any :singular :plural)
            :reader  number)))

(defword article (determiner)
  ((%number      :type    (member :singular :plural))
   (%determinate :initarg :determinate
                 :type    boolean
                 :reader  determinate)))

(defword quantifier (determiner) ())

(defword possessive-adjective (determiner)
  ((%gender    :initarg :gender
               :type    (member :any :masculine :feminine)
               :reader  gender)
   (%person    :initarg :person
               :type    (member :first :second :third)
               :reader  person)
   (%refnumber :initarg :refnumber
               :type    (member :any :singular :plural)
               :reader  refnumber)))

(defword demonstrative-adjective (determiner) ())

(defword interrogative-adjective (determiner) ())

(defword noun-verb-contraction (noun verb) ())

(defword verb-verb-contraction (verb) ())
