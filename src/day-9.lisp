(defpackage #:day-9
  (:use #:cl #:aoc)
  (:import-from #:intcode #:file->program #:run-program)
  (:export #:solution-1 #:solution-2))

(in-package #:day-9)

(defun read-input ()
  (file->program #p"day-9-input.txt"))

(defun solution-1 ()
  (let* ((program (read-input)))
    (run-program program :input '(1) :max-outputs 1)))

(defun solution-2 ()
  (let* ((program (read-input)))
    (run-program program :input '(2) :max-outputs 1)))
