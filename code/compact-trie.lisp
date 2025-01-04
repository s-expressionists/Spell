(cl:in-package #:spell)

;;; Internal protocol

(defgeneric compact (dictionary))

(defgeneric compact-node (node depth))

(defgeneric compact-node-slots (node depth)
  (:method-combination append))

(defgeneric compact-entry (entry))

;;; `compact-node'

(defclass compact-node (node) ())

;;; `compact-leaf-mixin', related types and leaf protocol methods

(deftype compact-entry ()
  'word)

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
    (map-entries (lambda (entry) (push entry entries))
                 object  (%entries object))
    `(((:entries (:after :children)) " entries: ~{~A~^ ~}" ,entries))))

(defmethod map-entries
    ((function function) (node compact-leaf-mixin) (entries t))
  (funcall function entries))

(defmethod map-entries
    ((function function) (node compact-leaf-mixin) (entries vector))
  (map nil function entries))

(defmethod %lookup ((function function)
                    (string   string)
                    (suffix   (eql 0))
                    (node     compact-leaf-mixin))
  (map-entries
   (lambda (word)
     ;; This is temporary: make an instance of the explicit base
     ;; "sibling" class.
     (let ((word (if (typep word 'implicit-base-mixin)
                     (let* ((class-name  (class-name (class-of word)))
                            (class-info  (find class-name *word-classes*
                                               :test #'eq :key #'third))
                            (new-class   (find-class (fourth class-info)))
                            (base-suffix (base-suffix word))
                            (base        (if (plusp base-suffix)
                                             (subseq string 0 (- (length string)
                                                                 base-suffix))
                                             string)))
                       (if (slot-exists-p (c2mop:class-prototype new-class) '%info)
                           (let ((new-info (ldb (byte 29 +base-suffix-bits+)
                                                (slot-value word '%info))))
                             (make-instance new-class :info new-info
                                                      :base base))
                           (make-instance new-class :base base)))
                     word)))
       (funcall function word)))
   node (%entries node)))

(defmethod compact-node-slots append ((node leaf-mixin) (depth integer))
  (flet ((compact-entry (entries) entries))
    (let ((compact-entries '()))
      (map-entries (lambda (entry)
                     (push (compact-entry entry) compact-entries))
                   node (%entries node))
      (let ((new-entries (if (a:length= 1 compact-entries)
                             (first compact-entries)
                             (coerce compact-entries 'vector))))
        (list :entries new-entries)))))

;;; `compact-interior-mixin'

(deftype child-key/character ()
  'character)

(deftype child-key/string ()
  'simple-string)

(deftype child-key ()
  '(or child-key/character child-key/string))

(deftype compact-child-cell ()
  '(cons child-key compact-node))

(defun %every-compact-child-chell (object)
  (and (typep object 'sequence)
       (let ((length (length object)))
         (and (zerop (mod length 2))
              (loop :for i :below length :by 2
                    :for key   = (aref object (+ i 0))
                    :for child = (aref object (+ i 1))
                    :always (and (typep key 'child-key)
                                 (typep child '(or null ; can happen temporarily while rebuilding from fasl
                                                   node))))))))

(deftype vector-of-compact-child-cell ()
  '(and (simple-array t 1) (satisfies %every-compact-child-chell)))

(deftype compact-child-cells ()
  '(or compact-child-cell vector-of-compact-child-cell))

(defclass compact-interior-mixin (interior-mixin)
  ((%children :initarg  :children
              :type     compact-child-cells)))

(defmethod map-children
    ((function function) (node compact-interior-mixin) (children cons))
  (destructuring-bind (key . child) children
    (funcall function key child)))

(defmethod map-children
    ((function function) (node compact-interior-mixin) (children vector))
  (loop :for i     :below (length children) :by 2
        :for key   =      (aref children (+ i 0))
        :for child =      (aref children (+ i 1))
        :do (funcall function key child)))

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
    (loop :with offset =      (- (length string) suffix)
          :for  i      :below (length children) :by 2
          :for  key    =      (aref children i)
          :do (consider-child-cell
               string suffix offset key (aref children (+ i 1))))))

(defmethod %lookup ((function function)
                    (string   string)
                    (suffix   integer)
                    (node     compact-interior-mixin))
  (multiple-value-bind (child progress)
      (find-child string suffix node (%children node))
    (when (not (null child))
      (%lookup function string (- suffix progress) child))))

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
                      (push (compact-child key child) compact-children))
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

(defmethod %lookup ((function function)
                    (string   string)
                    (suffix   (eql 0))
                    (nodef    compact-interior-node))
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

(defclass compact-leaf-node (compact-leaf-mixin compact-node) ())

(defmethod compact-node ((node raw-leaf-node) (depth integer))
  (apply #'make-instance 'compact-leaf-node
         (compact-node-slots node depth)))

(defclass compact-interior-leaf-node (compact-interior-mixin
                                      compact-leaf-mixin
                                      compact-node)
  ())

(defmethod compact-node ((node raw-interior-leaf-node) (depth integer))
  (apply #'make-instance 'compact-interior-leaf-node
         (compact-node-slots node depth)))
