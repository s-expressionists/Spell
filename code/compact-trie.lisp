(cl:in-package #:spell)

;;; Internal protocol

(defgeneric compact (dictionary))

(defgeneric compact-node (node depth))

(defgeneric compact-node-slots (node depth)
  (:method-combination append))

(defgeneric compact-entry (entry))

(defgeneric expand-entry (entry spelling))

;;; Entries

(deftype class-index+info ()
  `(unsigned-byte ,(+ +word-class-index-bits+ +info-bits+)))

(declaim (inline decode-class-info-and-info))
(defun decode-class-info-and-info (class-index+info)
  (let* ((class-index (ldb (byte +word-class-index-bits+ 0) class-index+info))
         (class-info  (aref *word-classes* class-index))
         (info        (ldb (byte +info-bits+ +word-class-index-bits+)
                           class-index+info)))
    (values class-info info)))

(defun encode-class-index+info (class info)
  (declare (type (unsigned-byte #.+info-bits+) info))
  (let ((class-index (position (class-name class) *word-classes*
                               :key (lambda (cell)
                                      (if (subtypep class 'explicit-base-mixin)
                                          (fourth cell)
                                          (third cell))))))
    (declare (type word-class-index class-index))
    (logior (ash info +word-class-index-bits+) class-index)))

(deftype compact-entry ()
  '(or class-index+info
       (cons class-index+info simple-string))) ; (CLASS-INDEX+INFO . BASE)

(defmethod compact-entry ((entry word))
  (let ((class (class-of entry))
        (info  (if (slot-exists-p entry '%info)
                   (slot-value entry '%info)
                   0)))
    (encode-class-index+info class info)))

(defmethod compact-entry ((entry explicit-base-mixin))
  (let ((info (call-next-method))
        (base (intern-string (base entry)))) ; TODO: could intern strings into a big array and add index to flags
    (cons info base)))

(defmethod expand-entry ((entry cons) (spelling string))
  (destructuring-bind (class-index+info . base) entry
    (declare (type class-index+info class-index+info))
    (multiple-value-bind (class-info info)
        (decode-class-info-and-info class-index+info)
      (let ((constructor (the function (second class-info))))
        (funcall constructor info base)))))

(defmethod expand-entry ((entry integer) (spelling string))
  (declare (type class-index+info entry))
  (multiple-value-bind (class-info info) (decode-class-info-and-info entry)
    (let* ((constructor (the function (second class-info)))
           (base-suffix (ldb (byte +base-suffix-bits+ 0) info))
           (base        (if (zerop base-suffix)
                            spelling
                            (subseq spelling 0 (- (length spelling)
                                                  base-suffix))))
           (info        (ldb (byte +info-bits+ +base-suffix-bits+) info)))
      (funcall constructor info base))))

;;; `compact-node'

(defclass compact-node (node) ())

;;; Occurs only for empty raw trie.
(defmethod compact-node ((node raw-node) (depth integer))
  (make-instance 'compact-node))

;;; `compact-leaf-mixin', related types and leaf protocol methods

(defun %every-compact-entry (object)
  (and (typep object 'sequence) (every (a:of-type 'compact-entry) object)))

(deftype vector-of-compact-entry ()
  '(and (simple-array * 1) (satisfies %every-compact-entry)))

(deftype compact-entries ()
  '(or compact-entry vector-of-compact-entry))

(defclass compact-leaf-mixin (leaf-mixin)
  ((%entries :initarg :entries
             :type    compact-entries)))

(defmethod utilities.print-items:print-items append
    ((object compact-leaf-mixin))
  (let ((entries '()))
    (map-leaf-entries (lambda (entry) (push entry entries))
                      object (%entries object))
    `(((:entries (:after :children)) " entries: ~{~A~^ ~}" ,entries))))

(defmethod map-leaf-entries
    ((function function) (node compact-leaf-mixin) (entries t))
  (funcall function entries))

(defmethod map-leaf-entries
    ((function function) (node compact-leaf-mixin) (entries vector))
  (map nil function entries))

(defmethod node-lookup ((function function)
                        (string   string)
                        (suffix   (eql 0))
                        (node     compact-leaf-mixin))
  (map-leaf-entries (lambda (entry)
                      (let ((word (expand-entry entry string)))
                        (funcall function word)))
                    node (%entries node)))

(defmethod map-node-entries ((function   function)
                             (node       compact-leaf-mixin)
                             (characters vector))
  (flet ((visit (entry) (funcall function entry characters)))
    (declare (dynamic-extent #'visit))
    (map-leaf-entries #'visit node (%entries node))))

(defmethod compact-node-slots append ((node leaf-mixin) (depth integer))
  (let ((compact-entries '()))
    (map-leaf-entries (lambda (entry)
                        (push (compact-entry entry) compact-entries))
                      node (%entries node))
    (let ((new-entries
            (cond ((a:length= 1 compact-entries)
                   (first compact-entries))
                  ((every (a:of-type 'unsigned-byte) compact-entries)
                   (let ((bit-count (reduce #'max compact-entries
                                            :key #'integer-length)))
                     (make-array (length compact-entries)
                                 :element-type     `(unsigned-byte ,bit-count)
                                 :initial-contents compact-entries)))
                  (t
                   (coerce compact-entries 'vector)))))
      (list :entries new-entries))))

;;; `compact-interior-mixin'

(deftype child-key/character ()
  'character)

(deftype child-key/string ()
  'simple-string)

(deftype child-key ()
  '(or child-key/character child-key/string))

(deftype compact-child-value ()
  '(or null ; can happen temporarily when rebuilding from fasl
       compact-node
       compact-entries))

(deftype compact-child-cell ()
  '(cons child-key compact-child-value))

(defun %every-compact-child-cell (object)
  (and (typep object 'sequence)
       (let ((length (length object)))
         (and (zerop (mod length 2))
              (loop :for i :below length :by 2
                    :for key   = (aref object (+ i 0))
                    :for child = (aref object (+ i 1))
                    :always (and (typep key 'child-key)
                                 (typep child 'compact-child-value)))))))

(deftype vector-of-compact-child-cell ()
  '(and (simple-array t 1) (satisfies %every-compact-child-cell)))

(deftype compact-child-cells ()
  '(or compact-child-cell vector-of-compact-child-cell))

(defclass compact-interior-mixin (interior-mixin)
  ((%children :initarg  :children
              :type     compact-child-cells)))

(defmethod map-children
    ((function function) (node compact-interior-mixin) (children cons))
  (destructuring-bind (key . child) children
    (multiple-value-bind (new-key new-child) (funcall function key child)
      (when (and new-key new-child)
        (setf (car children) new-key (cdr children) new-child)))))

(defmethod map-children
    ((function function) (node compact-interior-mixin) (children vector))
  (declare (type simple-vector children))
  (loop :for i     :below (length children) :by 2
        :for key   =      (aref children (+ i 0))
        :for child =      (aref children (+ i 1))
        :do (multiple-value-bind (new-key new-child)
                (funcall function key child)
              (when (and new-key new-child)
                (setf (aref children (+ i 0)) new-key
                      (aref children (+ i 1)) new-child)))))

(macrolet ((consider-child-cell (string suffix offset key child-expression)
             `(etypecase ,key
                (child-key/character
                 (when (char= ,key (aref ,string ,offset))
                   (return (values ,child-expression 1))))
                (child-key/string
                 (let* ((length (length ,key))
                        (end    (+ ,offset length)))
                   (when (and (<= length ,suffix)
                              (string= ,string ,key :start1 ,offset :end1 end))
                     (return (values ,child-expression length))))))))

  (defmethod find-child ((string   string)
                         (suffix   integer)
                         (node     compact-interior-mixin)
                         (children cons))
    (let ((offset (- (length string) suffix))
          (key    (car children)))
      (block nil
        (consider-child-cell string suffix offset key (cdr children)))))

  (defmethod find-child ((string   string)
                         (suffix   integer)
                         (node     compact-interior-mixin)
                         (children vector))
    (declare (type simple-vector children))
    (loop :with offset =      (- (length string) suffix)
          :for  i      :below (length children) :by 2
          :for  key    =      (aref children i)
          :do (consider-child-cell
               string suffix offset key (aref children (+ i 1))))))

(defmethod node-lookup ((function function)
                        (string   string)
                        (suffix   integer)
                        (node     compact-interior-mixin))
  (multiple-value-bind (child progress)
      (find-child string suffix node (%children node))
    (when (not (null child))
      (node-lookup function string (- suffix progress) child))))

(defmethod map-node-entries ((function   function)
                             (node       compact-interior-mixin)
                             (characters vector))
  (labels ((consider-child/char (key child)
             (vector-push-extend key characters)
             (map-node-entries function child characters)
             (vector-pop characters))
           (consider-child/string (key child)
             (let* ((key-length (length key))
                    (length     (length characters))
                    (required   (+ length key-length)))
               (when (< (array-total-size characters) required)
                 (adjust-array characters required))
               (incf (fill-pointer characters) key-length)
               (setf (subseq characters length) key)
               (map-node-entries function child characters)
               (decf (fill-pointer characters) key-length)))
           (consider-child (key child)
             (etypecase key
               (character (consider-child/char key child))
               (string    (consider-child/string key child)))
             ;; `map-children' can write back modifications, so avoid
             ;; returning anything that would be written back.
             nil))
    (declare (dynamic-extent #'consider-child/char
                             #'consider-child/string
                             #'consider-child))
    (map-children #'consider-child node (%children node)))
  ;; Now that children and compact entries have been handled, if NODE
  ;; is also a leaf, call the next method which is the one specialized
  ;; to `leaf-mixin' to handle the non-compact entries.
  (when (leafp node)
    (call-next-method)))

(defmethod compact-node-slots append ((node raw-interior-mixin) (depth integer))
  (labels ((make-key (key child-key)
             (intern-string
              (concatenate 'string (string key) (string child-key))))
           (compact-child (key child)
             ;; NEW-CHILD-KEY is non-NIL when the ancestors of
             ;; NEW-CHILD could be omitted because NEW-CHILD (and
             ;; potentially its ancestors) had no siblings. In that
             ;; case, coerce the whole path of keys from NODE to
             ;; NEW-CHILD into a string. The lookup will skip multiple
             ;; levels in one step compared to the raw trie.
             (multiple-value-bind (new-child new-child-key)
                 (compact-node child (1+ depth))
               (assert (not (null new-child)))
               (let ((new-key (cond ((not (null new-child-key))
                                     (make-key key new-child-key))
                                    ((stringp key)
                                     (intern-string key))
                                    (t
                                     key))))
                 (cons new-key new-child)))))
    (let ((compact-children '()))
      (map-children (lambda (key child)
                      (push (compact-child key child) compact-children)
                      nil)
                    node (%children node))
      (let* ((child-count  (length compact-children))
             (new-children
               (if (= child-count 1)
                   (first compact-children)
                   (let ((new-children (make-array (* 2 child-count)))
                         (sorted       (sort compact-children #'string<
                                             :key #'car)))
                     ;; Sorting is essential for the structure sharing
                     ;; optimization.
                     (loop :for (key . child) :in sorted
                           :for i :from 0 :by 2
                           :do (setf (aref new-children (+ i 0)) key
                                     (aref new-children (+ i 1)) child))
                     new-children))))
        (list :children new-children)))))

;;; Concrete node classes

(defclass compact-interior-node (compact-interior-mixin compact-node) ())

(defmethod leafp ((node compact-interior-node))
  nil)

(defmethod node-lookup ((function function)
                        (string   string)
                        (suffix   (eql 0))
                        (node     compact-interior-node))
  nil)

(defmethod compact-node ((node raw-interior-node) (depth integer))
  (let* ((initargs (compact-node-slots node depth))
         (children (getf initargs :children)))
    (if (and (not (zerop depth)) ; can't omit the root node
             (typep children 'compact-child-cell))
        ;; If NODE has only a single child, return that child and its
        ;; key but omit NODE so that the caller can create a
        ;; compressed path to CHILD.
        (destructuring-bind (key . child) children
          (values child key))
        (apply #'make-instance 'compact-interior-node initargs))))

;;; We would normally define `compact-leaf-node' here, but there is no
;;; `compact-leaf-node' class since `compact-node' for `raw-leaf-node'
;;; returns the entries directly instead of wrapping them in a leaf
;;; node. A lookup in there entries can only succeed if the suffix is
;;; already 0.

(defmethod node-lookup ((function function)
                        (string   string)
                        (suffix   integer)
                        (node     t))
  nil)

(defmethod node-lookup ((function function)
                        (string   string)
                        (suffix   (eql 0))
                        (node     t)) ; integer and cons
  (let ((word (expand-entry node string)))
    (funcall function word)))

(defmethod node-lookup ((function function)
                        (string   string)
                        (suffix   (eql 0))
                        (node     vector))
  (map nil (lambda (entry)
             (let ((word (expand-entry entry string)))
               (funcall function word)))
       node))

;;; The next two methods apply to the three kinds of compressed leaf
;;; nodes, namely vectors of "info" integers, conses of an "info"
;;; integer and a base string and just "info" integers.
(defmethod map-node-entries ((function   function)
                             (node       t) ; integer and cons
                             (characters vector))
  (funcall function node characters))

(defmethod map-node-entries ((function   function)
                             (node       vector)
                             (characters vector))
  (flet ((visit (entry) (funcall function entry characters)))
    (declare (dynamic-extent #'visit))
    (map nil #'visit node)))

(defmethod compact-node ((node raw-leaf-node) (depth integer))
  (let ((initargs (compact-node-slots node depth)))
    (assert (a:length= 2 initargs))
    (getf initargs :entries)))

(defclass compact-interior-leaf-node (compact-interior-mixin
                                      compact-leaf-mixin
                                      compact-node)
  ())

(defmethod compact-node ((node raw-interior-leaf-node) (depth integer))
  (apply #'make-instance 'compact-interior-leaf-node
         (compact-node-slots node depth)))
