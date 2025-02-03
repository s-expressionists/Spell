(cl:in-package #:spell)

;;; Dictionary protocol

(defgeneric entry-count (dictionary))

(defgeneric map-entries (function dictionary))

(defgeneric lookup (string dictionary))

(defgeneric map-similar (function string dictionary threshold))

(defgeneric map-corrections (function string dictionary threshold
                             &key variants group-by count))

(defgeneric corrections (string dictionary threshold
                         &key variants group-by count))

(defgeneric insert (object string dictionary))

(defgeneric load-dictionary (source &key into))

;;; Trie node protocols

;;; Lookup protocol

(defgeneric node-lookup (function string suffix node))

;;; Similar protocol

(defgeneric node-map-similar (function string suffix node threshold characters))

;;; Insert protocol

(defgeneric node-insert (object string suffix node))

;;; Leaf node protocol

(defgeneric map-leaf-entries (function node entries))

(defgeneric add-leaf-entry (entry node entries))

;;; Interior node protocol

(defgeneric map-children (function node children))

(defgeneric find-child (string suffix node children))

(defgeneric add-child (char child node children))
