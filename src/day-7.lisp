(defpackage #:day-7
  (:use #:cl #:split-sequence #:alexandria #:iterate #:aoc)
  (:import-from #:day-5 #:run-program-1 #:run-program)
  (:export #:solution-1 #:solution-2))

(in-package #:day-7)

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

(defparameter *amp-ips* (make-array 5))
(defparameter *amp-code* (make-array 5))

(defun run-cycle (phase-settings &optional (input 0))
  (iter
    (for i :below 5)
    (for ip = (svref *amp-ips* i))
    (for in = (if (zerop i) input out))
    (for (values next-ip out) = (run-program-1
				 (svref *amp-code* i)
				 (if (zerop ip) (list (nth i phase-settings) in) (list in))
				 ip))
    (setf (svref *amp-ips* i) next-ip)
    (finally (return (values next-ip out)))))

(defun run-until-halt (phase-settings)
  (iter
    (for (values next-ip out) = (run-cycle phase-settings p-out))
    (for p-out :previous out :initially 0)
    (while next-ip)
    (finally (return p-out))))

(defun run-amplifiers (phase-settings)
  (iter
    (for i :below 5)
    (setf (svref *amp-code* i) (copy-array *program*))
    (setf (svref *amp-ips* i) 0))
  (let ((result (run-until-halt phase-settings)))
    (when (< *result* result) (setq *result* result))))

(defun solution-2 ()
  (let ((*program* (read-code #p"day-7-input.txt"))
	(*result* 0))
    (map-permutations #'run-amplifiers '(5 6 7 8 9))
    *result*))
