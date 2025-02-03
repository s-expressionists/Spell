(cl:in-package #:spell)

;;; Dictionary

(defclass dictionary (utilities.print-items:print-items-mixin)
  ((%contents    :accessor contents
                 :initform (make-instance 'raw-node))
   (%entry-count :accessor entry-count
                 :initform 0)))

(defmethod make-load-form ((object dictionary) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod utilities.print-items:print-items append ((object dictionary))
  `((:entry-count "~:D entr~:@P" ,(entry-count object))))

(defmethod map-entries ((function t) (dictionary dictionary))
  (let ((function   (a:ensure-function function))
        ;; Since trie nodes do not contain the actual spellings of the
        ;; respective associated words, we keep track of the path to
        ;; the currently visited node in this array by pushing and
        ;; popping characters.
        (characters (make-array 20 :element-type 'character
                                   :adjustable   t
                                   :fill-pointer 0)))
    (flet ((visit (entry characters)
             (let* (;; We copy CHARACTERS since we don't want the base
                    ;; of WORD to change retroactively.
                    (spelling (copy-seq characters))
                    (word     (expand-entry entry spelling)))
               (funcall function spelling word))))
      (declare (dynamic-extent #'visit))
      (map-node-entries #'visit (contents dictionary) characters))))

(defmethod lookup ((string string) (dictionary dictionary))
  (let ((length (length string))
        (result '()))
    (node-lookup (lambda (word)
                   (push word result))
                 string length (contents dictionary))
    result))

(defmethod map-similar ((function   t)
                        (string     sequence)
                        (dictionary dictionary)
                        (threshold  integer))
  (check-type threshold a:array-index)
  (let ((function   (a:ensure-function function))
        (length     (length string))
        (seen       (make-hash-table :test #'equal))
        (characters (make-array 20 :element-type 'character
                                   :adjustable   t
                                   :fill-pointer 0)))
    (node-map-similar
     (lambda (entry characters remaining-threshold)
       (declare (type (and (not simple-array) (array character 1)) characters)
                (type a:array-index                                remaining-threshold))
       (let* ((spelling (copy-seq characters))
              (key      (cons spelling entry))
              (existing (gethash key seen)))
         (declare (type (or null a:array-index) existing))
         (when (or (null existing)
                   (> remaining-threshold existing))
           (setf (gethash key seen) remaining-threshold)
           (let* ((distance (- threshold remaining-threshold))
                  (word     (expand-entry entry spelling)))
             (funcall function word spelling distance)))))
     string length (contents dictionary) threshold characters))
  nil)

(defmethod map-corrections ((function   function)
                            (string     sequence)
                            (dictionary dictionary)
                            (threshold  integer)
                            &key (variants (lambda (continuation string)
                                             (funcall continuation string)))
                                 (group-by :entry)
                                 (count    nil))
  (check-type threshold a:array-index)
  (check-type group-by  (member :entry :spelling))
  (check-type count     (or null a:array-index))
  (let ((variants (a:ensure-function variants))
        (results  (make-hash-table :test #'equal))
        ;; The element at index I of COUNTS is the number of elements
        ;; in RESULTS for which the distance is I or better.  For
        ;; example, if (aref counts 1) is 5 and COUNT is 5, we know
        ;; that we have enough results with distance 1 or better.  We
        ;; can therefore discard any result the distance of which is 1
        ;; or more.
        (counts   (unless (null count)
                    (make-array (1+ threshold) :element-type    'a:array-index
                                               :initial-element 0))))
    (block collect
      (flet ((try (variant threshold)
               (declare (type a:array-index threshold))
               (map-similar
                (lambda (node spelling distance)
                  (declare (type a:array-index distance))
                  (when (or (null count)
                            (< (aref counts distance) count))
                    (let* ((entry    (cl:case group-by
                                       (:entry    (cons node spelling))
                                       (:spelling spelling)))
                           (existing (gethash entry results)))
                      (declare (type (or null a:array-index) existing))
                      (when (or (null existing) (< distance existing))
                        (setf (gethash entry results) distance)
                        (unless (null count)
                          (loop :for i :from distance :to threshold
                                :for new-count = (1+ (aref counts i))
                                :when (= new-count count)
                                  :do (return-from collect)
                                :do (setf (aref counts i) new-count)))))))
                variant dictionary threshold)))
        ;; If COUNT limits the size of the result, iterate with
        ;; increasing threshold since smaller thresholds are /a lot/
        ;; less work and may already generate the requested number of
        ;; results.
        (if (null count)
            (funcall variants (lambda (variant) (try variant threshold)) string)
            (loop :while (< (hash-table-count results) count)
                  :for i :of-type a:array-index :from 0 :to threshold
                  :do (funcall variants
                               (lambda (variant) (try variant i))
                               string)))))
    ;; Collect all results or just the requested number.  Start with
    ;; closer matching results.
    (loop :with sorted = (sort (a:hash-table-alist results) #'< :key #'cdr)
          :repeat (or count array-dimension-limit)
          :for (entry . distance) :in sorted
          :do (if (consp entry)
                  (funcall function (car entry) (cdr entry) distance)
                  (funcall function entry distance)))))

(defmethod corrections ((string     sequence)
                        (dictionary dictionary)
                        (threshold  integer)
                        &rest args &key (group-by :entry) variants count)
  (declare (ignore variants count))
  (let ((results  '())
        (correct? nil))
    (apply #'map-corrections
           (ecase group-by
             (:entry    (lambda (word spelling distance)
                          (when (zerop distance)
                            (setf correct? t))
                          (push (cons word spelling) results)))
             (:spelling (lambda (spelling distance)
                          (when (zerop distance)
                            (setf correct? t))
                          (push spelling results))))
           string dictionary threshold args)
    (values (nreverse results) correct?)))

(defmethod insert ((object t) (string string) (dictionary dictionary))
  (let ((root (contents dictionary)))
    (when (typep root 'compact-node)
      (error "~@<Cannot insert into compacted dictionary.~@:>"))
    (node-insert object string (length string) root)))

(defmethod compact ((dictionary dictionary))
  (setf (contents dictionary) (with-string-interning ()
                                (compact-node (contents dictionary) 0)))
  dictionary)

(defmethod share-structure ((dictionary dictionary) &rest args &key depth-limit)
  (declare (ignore depth-limit))
  (setf (contents dictionary) (apply #'share-structure (contents dictionary)
                                     args))
  dictionary)

;;; Dictionary loading

(defmethod load-dictionary ((source stream)
                            &key (into (make-instance 'dictionary)))
  (let ((count 0))
    (with-string-interning ()
      (map-dictionary-file-entries
       (lambda (spelling type base &rest initargs)
         (let* ((spelling (intern-string spelling))
                (base     (intern-string base))
                (word     (apply #'make-word spelling type base initargs)))
           (insert word spelling into))
         (incf count))
       source))
    (incf (entry-count into) count)
    into))

(defmethod load-dictionary ((source pathname) &rest args &key into)
  (declare (ignore into))
  (with-open-file (stream source)
    (apply #'load-dictionary stream args)))

(defmethod load-dictionary ((source string) &rest args &key into)
  (declare (ignore into))
  (apply #'load-dictionary (pathname source) args))

(defmethod load-dictionary ((source sequence)
                            &key (into (make-instance 'dictionary)))
  (mapc (lambda (source)
          (with-simple-restart (skip "Skip source ~A" source)
            (apply #'load-dictionary source :into into)))
        source))
