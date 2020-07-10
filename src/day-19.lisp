(defpackage #:day-19
  (:use #:cl #:aoc #:iterate #:alexandria)
  (:import-from #:day-5 #:run-program-1 #:run-program-collect-results #:allocate-program-memory)
  (:export #:solution-1 #:solution-2))

(in-package #:day-19)

(defconstant +map-size+ 50)
(defconstant +square-size+ 100)
(define-constant +beam-start-scan+ (cons 5 4) :test #'equal)

(defun read-input ()
  (read-code (resource-file #p"day-19-input.txt")))

(defvar *program*)

(defun scan-map ()
  (iter outer
    (for x :below +map-size+)
    (iter
      (for y :below +map-size+)
      (for result = (scan-location x y))
      (in outer (counting result)))))

(defun solution-1 ()
  (let ((*program* (allocate-program-memory (read-input) 600)))
    (scan-map)))

(defparameter *scan-location-cache* (make-hash-table))

(defun scan-location (x y)
  (memoize-function *scan-location-cache* (logior (ash x 16) y)
    (->> 0
      (run-program-1 (copy-array *program*) (list x y))
      (multiple-value-list)
      (second)
      (eq 1))))

(defun test-location (x y)
  (iter outer
    (for dx :below +square-size+)
    (iter
      (for dy :below +square-size+)
      (in outer (counting (scan-location (+ dx x) (+ dy y)))))))

(defun next-row-start-point (point)
  (destructuring-bind (start-x . start-y) point
    (iter
      (with y = (1+ start-y))
      (for x :downfrom (1+ start-x))
      (for v = (scan-location x y))
      (for prev-v :previous v)
      (for prev-x :previous x)
      (until (and (null v) prev-v))
      (finally (return (cons prev-x y))))))

(defvar *squares-map*)

(defun find-square (start-point)
  (iter
    (with next-point = (next-row-start-point start-point))
    (with y = (cdr next-point))
    (for x :from (car next-point))
    (for v = (scan-location x y))
    (while v)
    (for size = (1+ (min
		     (gethash (cons (1- x) y) *squares-map* 0)
		     (gethash (cons x (1- y)) *squares-map* 0))))
    (setf (gethash (cons x y) *squares-map*) size)
    (when (>= size +square-size+)
     (leave
      (cons (- x (1- +square-size+)) (- y (1- +square-size+)))))
    (finally (return (find-square (cons x y))))))

(defun solution-2 ()
  (let* ((*program* (allocate-program-memory (read-input) 600))
	 (*squares-map* (make-hash-table :test #'equal)))
    (destructuring-bind (x . y) (find-square +beam-start-scan+)
      (assert (eql (test-location x y) (expt +square-size+ 2)))
      (+ (* 10000 x) y))))
