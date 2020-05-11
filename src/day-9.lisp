(defpackage :day-9
  (:use :cl :advent-of-code)
  (:import-from :day-5 :run-program :allocate-program-memory :run-program-collect-results)
  (:export :solution-1 :solution-2))

(in-package :day-9)

(defun read-input ()
  (read-code (resource-file #p"day-9-input.txt")))

(defun solution-1 ()
  (let* ((input (read-input)))
    (run-program-collect-results (allocate-program-memory input 1200) '(1))))

(defun solution-2 ()
  (let* ((input (read-input)))
    (run-program-collect-results (allocate-program-memory input 1200) '(2))))
