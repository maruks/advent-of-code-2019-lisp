(defpackage #:day-7
  (:use #:cl #:aoc)
  (:import-from #:alexandria #:if-let #:curry)
  (:import-from #:iterate #:iter #:for #:finally #:while)
  (:import-from #:intcode #:file->program #:run-program #:copy-code #:program-ip #:program-status)
  (:export #:solution-1 #:solution-2))

(in-package #:day-7)

(defun read-input ()
  (file->program #p"day-7-input.txt"))

(defun run-amplifier (program phase-settings &optional (input 0))
  (if-let (phase (car phase-settings))
    (run-amplifier program (cdr phase-settings) (run-program (copy-code program) :input (list phase input) :max-outputs 1))
    input))

(defun permutations (inputs &optional result)
  (if inputs
      (mapcan (λ (e) (permutations (remove e inputs) (cons e result))) inputs)
      (list result)))

(defun solution-1 ()
  (->> '(0 1 2 3 4)
    permutations
    (mapcar (curry #'run-amplifier (read-input)))
    (reduce #'max)))

(defun run-cycle (phase-settings amp-code &optional (input 0))
  (iter
    (for i :below 5)
    (for in = (if (zerop i) input out))
    (for out = (run-program
		(svref amp-code i)
		:input (if (zerop (program-ip (svref amp-code i)))
			   (list (nth i phase-settings) in)
			   (list in))
		:max-outputs 1))
    (finally (return out))))

(defun run-until-halt (phase-settings amp-code)
  (iter
    (for out = (run-cycle phase-settings amp-code p-out))
    (for p-out :previous out :initially 0)
    (while out)
    (finally (return p-out))))

(defun run-amplifiers (program phase-settings)
  (let* ((amplifiers 5)
	 (amp-code (make-array amplifiers)))
    (iter
      (for i :below amplifiers)
      (setf (svref amp-code i) (copy-code program)))
    (run-until-halt phase-settings amp-code)))

(defun solution-2 ()
  (->> '(5 6 7 8 9)
    permutations
    (mapcar (curry #'run-amplifiers (read-input)))
    (reduce #'max)))
