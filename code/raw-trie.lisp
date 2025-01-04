(cl:in-package #:spell)

;;; `raw-node' class

(defclass raw-node (node) ())

(defmethod %insert ((object t) (string string) (suffix (eql 0)) (node raw-node))
  (change-class node 'raw-leaf-node)
  (%insert object string 0 node))

(defmethod %insert ((object t) (string string) (suffix integer) (node raw-node))
  (change-class node 'raw-interior-node)
  (%insert object string suffix node))

;;; `raw-leaf-mixin' class and leaf node protocol methods

(defclass raw-leaf-mixin (leaf-mixin)
  ((%entries :type   simple-vector
             :writer (setf %entries))))

(defmethod utilities.print-items:print-items append ((object raw-leaf-mixin))
  (let ((entries '()))
    (map-entries (lambda (entry)
                   (push (class-name (class-of entry)) entries))
                 object  (%entries object))
    `(((:entries (:after :children)) " entries: ~{~A~^ ~}" ,entries))))

(defmethod map-entries
    ((function function) (node raw-leaf-mixin) (entries vector))
  (map nil function entries))

(defmethod add-entry ((entry t) (node raw-leaf-mixin) (entries vector))
  (concatenate (class-of entries) (list entry) entries))

#-minimal-raw-trie
(defmethod %lookup
    ((function function) (string string) (suffix (eql 0)) (node raw-leaf-mixin))
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

(defmethod %insert
    ((object t) (string string) (suffix (eql 0)) (node raw-leaf-mixin))
  (setf (%entries node) (add-entry object node (%entries node))))

(defmethod %insert
    ((object t) (string string) (suffix integer) (node raw-leaf-mixin))
  (change-class node 'raw-interior-leaf-node)
  (%insert object string suffix node))

;;; `raw-interior-mixin' class and interior node protocol methods

(defclass raw-interior-mixin (interior-mixin)
  ((%children :type   simple-vector
              :writer (setf %children))))

(defmethod map-children
    ((function function) (node raw-interior-mixin) (children vector))
  (loop :for i     :below (length children) :by 2
        :for key   =      (aref children (+ i 0))
        :for child =      (aref children (+ i 1))
        :do (funcall function key child)))

(defmethod find-child
    ((char character) (node raw-interior-mixin) (children vector))
  (loop :for i   :below (length children) :by 2
        :for key =      (aref children i)
        :when (char= key char)
          :do (return (aref children (1+ i)))))

(defmethod add-child
    ((char character) (child t) (node raw-interior-mixin) (children vector))
  (concatenate (class-of children) (list char child) children))

#-minimal-raw-trie
(defmethod %lookup
    ((function function) (string string) (suffix integer) (node raw-interior-mixin))
  (let* ((character (aref string (- (length string) suffix)))
         (child     (find-child character node (%children node))))
    (when (not (null child))
      (%lookup function string (1- suffix) child))))

(defmethod %insert
    ((object t) (string string) (suffix integer) (node raw-interior-mixin))
  (let* ((character (aref string (- (length string) suffix)))
         (children  (%children node))
         (child     (find-child character node children)))
    (when (null child)
      (setf child            (make-instance 'raw-node)
            (%children node) (add-child character child node children)))
    (%insert object string (1- suffix) child)))

;;; Concrete node classes

(defclass raw-interior-node (raw-interior-mixin interior-node raw-node) ())

#-minimal-raw-trie
(defmethod %lookup
    ((function function) (string string) (suffix (eql 0)) (node raw-interior-node))
  nil)

(defmethod %insert
    ((object t) (string string) (suffix (eql 0)) (node raw-interior-node))
  (change-class node 'raw-interior-leaf-node)
  (%insert object string 0 node))

(defclass raw-leaf-node (raw-leaf-mixin leaf-node node) ())

(defclass raw-interior-leaf-node (raw-interior-mixin
                                  raw-leaf-mixin
                                  interior-leaf-node
                                  raw-node)
  ())
