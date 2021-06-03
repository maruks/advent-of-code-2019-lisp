(defpackage #:day-5
  (:use #:cl #:iterate #:aoc #:intcode)
  (:import-from #:intcode #:file->program #:run-program)
  (:export #:solution-1 #:solution-2))

(in-package #:day-5)

(defun read-input ()
  (file->program #p"day-5-input.txt"))

(defun solution-1 ()
  (-> (read-input)
    (run-program :input '(1))
    last
    car))

(defun solution-2 ()
  (-> (read-input)
    (run-program :input '(5))
    car))
