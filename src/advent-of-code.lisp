(defpackage :advent-of-code
  (:use :cl :uiop/stream :split-sequence :iterate)
  (:import-from :ppcre :create-scanner)
  (:export read-file read-lines read-string read-code resource-file sort-by-distance-fn
	   make-point point-x point-y manhattan-distance distance compare-points λ))

(in-package :advent-of-code)

(defparameter *resources* (asdf/system:system-relative-pathname :advent-of-code-2019 "resources/"))

(defmacro λ (&whole whole args &body body)
  (declare (ignore args body))
  (cons 'lambda (cdr whole)))

(defun resource-file (p)
  (merge-pathnames p *resources*))

(defun read-file (file)
  (read-file-lines (resource-file file)))

(defun read-lines (file &optional (parse-fn #'identity))
  (mapcar parse-fn
	  (read-file-lines (resource-file file))))

(defun read-string (parse-fn file &key (split-char #\,))
  (mapcar parse-fn
	  (split-sequence split-char (read-file-string (resource-file file)))))

(defun read-code (file)
  (let* ((input (read-string #'parse-integer file)))
    (apply #'vector input)))

(defstruct point x y)

(defun manhattan-distance (point-1 point-2)
  (+ (abs (- (point-x point-1) (point-x point-2))) (abs (- (point-y point-1) (point-y point-2)))))

(defun distance (point-1 point-2)
  (sqrt (+ (expt (- (point-x point-1) (point-x point-2)) 2)
	   (expt (- (point-y point-1) (point-y point-2)) 2))))

(defun sort-by-distance-fn (from)
  (λ (p1 p2)
    (< (distance from p1) (distance from p2))))

(defun compare-points (point-1 point-2)
  (or (< (point-x point-1) (point-x point-2)) (< (point-y point-1) (point-y point-2))))

;; regex #r macro
(defun regex-reader (stream char-1 char-2)
  (declare (ignore char-1))
  (declare (ignore char-2))
  `(create-scanner ,(read stream t nil t)))

(set-dispatch-macro-character #\# #\r #'regex-reader)
