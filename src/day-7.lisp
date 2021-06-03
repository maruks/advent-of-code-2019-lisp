(defpackage #:day-7
  (:use #:cl #:split-sequence #:alexandria #:iterate #:aoc)
  (:import-from #:intcode #:file->program #:run-program #:copy-code #:program-ip #:program-status)
  (:export #:solution-1 #:solution-2))

(in-package #:day-7)

(defun read-input ()
  (file->program #p"day-7-input.txt"))

(defvar *program*)
(defvar *result*)

(defun run-amplifier (phase-settings &optional (input 0))
  (if-let (phase (car phase-settings))
    (run-amplifier (cdr phase-settings) (run-program (copy-code *program*) :input (list phase input) :max-outputs 1))
    (when (< *result* input) (setq *result* input))))

(defun solution-1 ()
  (let ((*program* (read-input))
	(*result* 0))
    (map-permutations #'run-amplifier '(0 1 2 3 4))
    *result*))

(defparameter *amp-ips* (make-array 5))
(defparameter *amp-code* (make-array 5))

(defun run-cycle (phase-settings &optional (input 0))
  (iter
    (for i :below 5)
    (for in = (if (zerop i) input out))
    (for out = (run-program
		(svref *amp-code* i)
		:input (if (zerop (program-ip (svref *amp-code* i)))
			   (list (nth i phase-settings) in)
			   (list in))
		:max-outputs 1))
    (finally (return out))))

(defun run-until-halt (phase-settings)
  (iter
    (for out = (run-cycle phase-settings p-out))
    (for p-out :previous out :initially 0)
    (while out)
    (finally (return p-out))))

(defun run-amplifiers (phase-settings)
  (iter
    (for i :below 5)
    (setf (svref *amp-code* i) (copy-code *program*))
  ;;  (setf (svref *amp-ips* i) 0)
    )
  (let ((result (run-until-halt phase-settings)))
    (when (< *result* result) (setq *result* result))))

(defun solution-2 ()
  (let ((*program* (read-input))
	(*result* 0))
    (map-permutations #'run-amplifiers '(5 6 7 8 9))
    *result*))
