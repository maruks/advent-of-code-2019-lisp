(defpackage :advent-of-code
  (:use :cl :uiop/stream :split-sequence :iterate )
  (:export read-file read-lines read-string read-code resource-file
	   make-point point-x point-y distance compare-points λ))

(in-package :advent-of-code)

(defparameter *resources* (asdf/system:system-relative-pathname :advent-of-code-2019 "resources/"))

(defun resource-file (p)
  (merge-pathnames p *resources*))

(defun read-file (file)
  (read-file-lines (resource-file file)))

(defun read-lines (parse-fn file)
  (mapcar parse-fn
	  (read-file-lines (resource-file file))))

(defun read-string (parse-fn file &key (split-char #\,))
  (mapcar parse-fn
	  (split-sequence split-char (read-file-string (resource-file file)))))

(defun read-code (file)
  (let* ((input (read-string #'parse-integer file)))
    (apply #'vector input)))

(defstruct (point) x y)

(defun distance (point-1 point-2)
  (+  (abs (- (point-x point-1) (point-x point-2))) (abs (- (point-y point-1) (point-y point-2)))))

(defun compare-points (point-1 point-2)
  (or (< (point-x point-1) (point-x point-2)) (< (point-y point-1) (point-y point-2))))

(defmacro λ (&whole whole args &body body)
  (declare (ignore args body))
  (cons 'lambda (cdr whole)))
