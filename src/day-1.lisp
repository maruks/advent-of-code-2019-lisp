(defpackage :day-1
  (:use :cl :iterate :advent-of-code)
  (:export :solution-1 :solution-2 :fuel :fuel-2))

(in-package :day-1)

(defun fuel (n)
  (- (truncate n 3) 2))

(defun solution-1 ()
  (let* ((input (read-file #'parse-integer #p"day-1-input.txt")))
    (reduce #'+ (mapcar #'fuel input) :initial-value 0)))

(defun fuel-2 (n)
  (iterate
    (initially (setq i n))
    (for i next (fuel i))
    (while (plusp i))
    (sum i)))

(defun solution-2 ()
  (let* ((input (read-file #'parse-integer #p"day-1-input.txt")))
    (reduce #'+ (mapcar #'fuel-2 input) :initial-value 0)))
