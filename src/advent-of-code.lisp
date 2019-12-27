(defpackage :advent-of-code
  (:use :cl :uiop/stream :split-sequence)
  (:export read-lines read-string))

(in-package :advent-of-code)

(defparameter *resources* (asdf/system:system-relative-pathname :advent-of-code-2019 "resources/"))

(defun resource-file (p)
  (merge-pathnames p *resources*))

(defun read-lines (parse-fn file)
  (mapcar parse-fn
	  (read-file-lines (resource-file file))))

(defun read-string (parse-fn file &key (split-char #\,))
  (mapcar parse-fn
	  (split-sequence split-char (read-file-string (resource-file file)))))
