(in-package #:spell)

(defparameter *word-types* (make-hash-table :test #'eq))

(defclass word ()
  ((%base :initarg :base :reader base)))

(defmethod make-load-form ((object word) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmacro defword (class-name &body body)
  (let ((type (intern (symbol-name class-name) :keyword)))
    `(progn (setf (gethash ,type *word-types*) ',class-name)
            (defclass ,class-name ,@body))))

(defword noun (word)
  ((%number :initarg :number :reader number)
   (%case :initarg :case :initform :nominative :reader case)
   (%gender :initarg :gender :initform :any :reader gender)))

(defword proper-noun (noun) ())

(defword verb (word)
  ((%person :initform :any :initarg :person :reader person)
   (%number :initform :any :initarg :number :reader number)
   (%tense :initarg :tense :reader tense :initform nil)
   (%negative :initarg :negative :initform nil :reader negative)
   (%contraction :initarg :contraction :initform nil :reader contraction)
   (%strength :initarg :strength :initform :weak :reader strength)
   (%infinitive :initarg :infinitive :reader infinitive :initform nil)))

(defword preposition (word) ())

(defword adjective (word)
  ((%degree :initarg :degree :reader degree)))

(defword adverb (word) ())

(defword pronoun (word)
  ((%person :initarg :person :reader person)
   (%number :initarg :number :reader number)
   (%gender :initarg :gender :reader gender)
   (%case :initarg :case :initform :nominative :reader case)
   (%negative :initarg :negative :initform nil :reader negative)))

(defword personal-pronoun (pronoun) ())

(defword possessive-pronoun (pronoun)
  ((%refnumber :initform :any :initarg :refnumber :reader refnumber)))

(defword reflexive-pronoun (pronoun) ())

(defword demonstrative-pronoun (pronoun) ())

(defword interjection (word) ())

(defword conjunction (word) ())

(defword subordinate (conjunction) ())

(defword determiner (word)
  ((%number :initform :any :initarg :number :reader number)))

(defword article (determiner)
  ((%number :initarg :number :reader number)
   (%determinate :initform nil :initarg :determinate :reader determinate)))

(defword quantifier (determiner) ())

(defword possessive-adjective (determiner)
  ((%gender :initform :any :initarg :gender :reader gender)
   (%person :initarg :person :reader person)
   (%refnumber :initform :any :initarg :refnumber :reader refnumber)))

(defword demonstrative-adjective (determiner) ())

(defword interrogative-adjective (determiner) ())

(defword noun-verb-contraction (noun verb) ())

(defword verb-verb-contraction (verb) ())

(defun word (spelling type &rest initargs &key &allow-other-keys)
  (declare (ignore spelling))
  (let ((class (gethash type *word-types*)))
    (apply #'make-instance class initargs)))
