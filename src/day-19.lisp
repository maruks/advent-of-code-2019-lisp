(defpackage :day-19
  (:use :cl :advent-of-code :iterate :alexandria)
  (:import-from :day-5 :run-program-1 :run-program-collect-results :allocate-program-memory)
  (:export :solution-1 :solution-2))

(in-package :day-19)

(defconstant +map-size+ 50)

(defun read-input ()
  (read-code (resource-file #p"day-19-input.txt")))

(defun scan-map (program map)
  (iter outer
    (for x :below +map-size+)
    (iter
      (for y :below +map-size+)
      (multiple-value-bind (next-ip result) (run-program-1 (copy-array program) (list x y) 0)
	(declare (ignore next-ip))
        (setf (gethash (cons x y) map) (if (zerop result) #\. #\#))
	(in outer (counting (eq 1 result) ))))))

(defun solution-1 ()
  (let ((program (allocate-program-memory (read-input)))
	(map (make-hash-table :test #'equal)))
    (scan-map program map)))

(defun solution-2 ()
  )
