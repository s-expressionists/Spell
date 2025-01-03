(in-package #:spell)

;;; Dictionary protocol

(defgeneric entry-count (dictionary))

(defgeneric lookup (string dictionary))

(defgeneric insert (object string dictionary))

(defgeneric load-dictionary (source &key into))

;;; Trie node protocol

(defgeneric entries (node))

(defgeneric find-child (char entries))

(defgeneric add-child (node char entries))
