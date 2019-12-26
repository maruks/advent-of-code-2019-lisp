(defpackage :advent-of-code
  (:use :cl :uiop/stream)
  (:export read-file))

(in-package :advent-of-code)

(defparameter *resources* (asdf/system:system-relative-pathname :advent-of-code-2019 "resources/"))

(defun resource-file (p)
  (merge-pathnames p *resources*))

(defun read-file (parse-fn file)
  (mapcar parse-fn
	  (read-file-lines (resource-file file))))
