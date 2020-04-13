(defpackage :day-7
  (:use :cl :split-sequence :alexandria :advent-of-code)
  (:import-from :day-5 :run-program)
  (:export :solution-1 ))

(in-package :day-7)

(defvar *program*)
(defvar *result*)

(defun run-amplifier (phase-settings &optional (input 0))
  (if-let (phase (car phase-settings))
    (run-amplifier (cdr phase-settings) (run-program (copy-array *program*) (list phase input)))
    (when (< *result* input) (setq *result* input))))

(defun solution-1 ()
  (let ((*program* (read-code #p"day-7-input.txt"))
	(*result* 0))
    (map-permutations #'run-amplifier '(0 1 2 3 4))
    *result*))
