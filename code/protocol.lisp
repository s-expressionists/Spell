(in-package #:spell)

;;; Dictionary protocol

(defgeneric lookup (string dictionary))

(defgeneric insert (object string dictionary))

;;; Trie node protocol

(defgeneric entries (node))

(defgeneric find-child (char entries))

(defgeneric add-child (node char entries))
