(cl:in-package #:spell)

;;; Dictionary protocol

(defgeneric entry-count (dictionary))

(defgeneric lookup (string dictionary))

(defgeneric insert (object string dictionary))

(defgeneric load-dictionary (source &key into))

;;; Trie node protocols

;;; Lookup protocol

(defgeneric %lookup (function string suffix node))

;;; Insert protocol

(defgeneric %insert (object string suffix node))

;;; Leaf node protocol

(defgeneric map-entries (function node entries))

(defgeneric add-entry (entry node entries))

;;; Interior node protocol

(defgeneric map-children (function node children))

(defgeneric find-child (string suffix node children))

(defgeneric add-child (char child node children))
