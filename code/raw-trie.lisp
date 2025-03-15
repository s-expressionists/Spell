(cl:in-package #:spell)

;;; `raw-node' class

(defclass raw-node (node) ())

(defmethod node-insert ((object t)
                        (string string)
                        (suffix (eql 0))
                        (node   raw-node))
  (change-class node 'raw-leaf-node)
  (node-insert object string 0 node))

(defmethod node-insert ((object t)
                        (string string)
                        (suffix integer)
                        (node   raw-node))
  (change-class node 'raw-interior-node)
  (node-insert object string suffix node))

;;; `raw-leaf-mixin' class and leaf node protocol methods

;;; This definition is also used by the compact trie.  This definition
;;; cannot go into word-classes.lisp (where it would logically fit)
;;; because then it would potentially be evaluated too early, namely
;;; before the word classes from which the value is computed get
;;; registered.
(defconstant +info-bits+
  (flet ((max-info (class-name)
           (reduce #'max (fields (find-class class-name))
                   :key           #'bitfield:bitfield-slot-end
                   :initial-value 0)))
    (loop :for (nil nil class1 class2) :across *word-classes*
          :when (not (null class1))
          :maximizing (max (max-info class1) (max-info class2)))))

(defclass raw-leaf-mixin (leaf-mixin)
  ((%entries :type   simple-vector
             :writer (setf %entries))))

(defmethod utilities.print-items:print-items append ((object raw-leaf-mixin))
  (let ((entries '()))
    (map-leaf-entries (lambda (entry)
                        (push (class-name (class-of entry)) entries))
                      object (%entries object))
    `(((:entries (:after :children)) " entries: ~{~A~^ ~}" ,entries))))

(defmethod map-leaf-entries
    ((function function) (node raw-leaf-mixin) (entries vector))
  (declare (type simple-vector entries))
  (map nil function entries))

(defmethod add-leaf-entry ((entry t) (node raw-leaf-mixin) (entries vector))
  (declare (type simple-vector entries))
  (let ((new-entries (make-array (+ 1 (length entries)))))
    (setf (aref new-entries 0)   entry
          (subseq new-entries 1) entries)
    new-entries))

#-minimal-raw-trie
(defmethod node-lookup
    ((function function) (string string) (suffix (eql 0)) (node raw-leaf-mixin))
  (map-leaf-entries
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
                           (let ((new-info (ldb (byte (- +info-bits+
                                                         +base-suffix-bits+)
                                                      +base-suffix-bits+)
                                                (slot-value word '%info))))
                             (make-instance new-class :info new-info
                                                      :base base))
                           (make-instance new-class :base base)))
                     word)))
       (funcall function word)))
   node (%entries node)))

(defmethod node-insert
    ((object t) (string string) (suffix (eql 0)) (node raw-leaf-mixin))
  (setf (%entries node) (add-leaf-entry object node (%entries node))))

(defmethod node-insert
    ((object t) (string string) (suffix integer) (node raw-leaf-mixin))
  (change-class node 'raw-interior-leaf-node)
  (node-insert object string suffix node))

;;; `raw-interior-mixin' class and interior node protocol methods

(defclass raw-interior-mixin (interior-mixin)
  ((%children :type   simple-vector
              :writer (setf %children))))

(defmethod map-children
    ((function function) (node raw-interior-mixin) (children vector))
  (declare (type simple-vector children))
  (loop :for i     :below (length children) :by 2
        :for key   =      (aref children (+ i 0))
        :for child =      (aref children (+ i 1))
        :do (funcall function key child)))

(defmethod find-child ((string   string)
                       (suffix   integer)
                       (node     raw-interior-mixin)
                       (children vector))
  (declare (type simple-vector children))
  (loop :with char =      (aref string (- (length string) suffix))
        :for i     :below (length children) :by 2
        :for key   =      (aref children i)
        :when (char= key char)
          :do (return (aref children (1+ i)))))

(defmethod add-child
    ((char character) (child t) (node raw-interior-mixin) (children vector))
  (declare (type simple-vector children))
  (let ((new-children (make-array (+ 2 (length children)))))
    (setf (aref new-children 0)   char
          (aref new-children 1)   child
          (subseq new-children 2) children)
    new-children))

#-minimal-raw-trie
(defmethod node-lookup
    ((function function) (string string) (suffix integer) (node raw-interior-mixin))
  (a:when-let ((child (find-child string suffix node (%children node))))
    (node-lookup function string (1- suffix) child)))

(defmethod node-insert
    ((object t) (string string) (suffix integer) (node raw-interior-mixin))
  (let* ((children  (%children node))
         (child     (find-child string suffix node children)))
    (when (null child)
      (let ((character (aref string (- (length string) suffix))))
        (setf child            (if (> suffix 1)
                                   (make-instance 'raw-interior-node)
                                   (make-instance 'raw-leaf-node))
              (%children node) (add-child character child node children))))
    (node-insert object string (1- suffix) child)))

;;; Concrete node classes

(defclass raw-interior-node (raw-interior-mixin interior-node raw-node) ())

(defmethod leafp ((node raw-interior-node))
  nil)

#-minimal-raw-trie
(defmethod node-lookup
    ((function function) (string string) (suffix (eql 0)) (node raw-interior-node))
  nil)

(defmethod node-insert
    ((object t) (string string) (suffix (eql 0)) (node raw-interior-node))
  (change-class node 'raw-interior-leaf-node)
  (node-insert object string 0 node))

(defclass raw-leaf-node (raw-leaf-mixin leaf-node node) ())

(defclass raw-interior-leaf-node (raw-interior-mixin
                                  raw-leaf-mixin
                                  interior-leaf-node
                                  raw-node)
  ())
