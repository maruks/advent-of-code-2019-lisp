(defpackage #:day-19
  (:use #:cl #:aoc)
  (:import-from #:intcode #:file->program #:run-program #:copy-code)
  (:import-from #:alexandria #:define-constant)
  (:import-from #:iterate #:iter #:for #:counting #:in #:with #:until #:finally #:leave #:while)
  (:export #:solution-1 #:solution-2))

(in-package #:day-19)

(define-constant +map-size+ 50)
(define-constant +square-size+ 100)
(define-constant +beam-start-scan+ (cons 5 4) :test #'equal)

(defun read-input ()
  (file->program #p"day-19-input.txt"))

(defun scan-map (program)
  (iter outer
    (for x :below +map-size+)
    (iter
      (for y :below +map-size+)
      (for result = (scan-location program x y))
      (in outer (counting result)))))

(defun solution-1 ()
  (-> (read-input)
    scan-map))

(defparameter *scan-location-cache* (make-hash-table))

(defun scan-location (program x y)
  (memoize-function *scan-location-cache* (logior (ash x 16) y)
    (->> (run-program (copy-code program) :input (list x y) :max-outputs 1)
      (= 1))))

(defun test-location (program x y)
  (iter outer
    (for dx :below +square-size+)
    (iter
      (for dy :below +square-size+)
      (in outer (counting (scan-location program (+ dx x) (+ dy y)))))))

(defun next-row-start-point (program point)
  (destructuring-bind (start-x . start-y) point
    (iter
      (with y = (1+ start-y))
      (for x :downfrom (1+ start-x))
      (for v = (scan-location program x y))
      (for prev-v :previous v)
      (for prev-x :previous x)
      (until (and (null v) prev-v))
      (finally (return (cons prev-x y))))))

(defun find-square (program squares-map start-point)
  (iter
    (with next-point = (next-row-start-point program start-point))
    (with y = (cdr next-point))
    (for x :from (car next-point))
    (for v = (scan-location program x y))
    (while v)
    (for size = (1+ (min
		     (gethash (cons (1- x) y) squares-map 0)
		     (gethash (cons x (1- y)) squares-map 0))))
    (setf (gethash (cons x y) squares-map) size)
    (when (>= size +square-size+)
     (leave
      (cons (- x (1- +square-size+)) (- y (1- +square-size+)))))
    (finally (return (find-square program squares-map (cons x y))))))

(defun solution-2 ()
  (let* ((program (read-input))
	 (squares-map (make-hash-table :test #'equal)))
    (destructuring-bind (x . y) (find-square program squares-map +beam-start-scan+)
      (assert (eql (test-location program x y) (expt +square-size+ 2)))
      (+ (* 10000 x) y))))
