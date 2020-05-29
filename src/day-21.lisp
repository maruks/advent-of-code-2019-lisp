(defpackage :day-21
  (:use :cl :advent-of-code :iterate :alexandria)
  (:import-from :day-5 :run-program-1 :run-program-collect-results :allocate-program-memory)
  (:import-from :str :concat :add-suffix)
  (:export :solution-1 :solution-2))

(in-package :day-21)

(defun read-input ()
  (read-code (resource-file #p"day-21-input.txt")))

(defvar *program*)

(defun run-droid-code (code)
  (let ((input (->> (coerce (apply #'concat (add-suffix code (string #\Newline))) 'list)
		 (mapcar #'char-code))))
    (run-program-collect-results *program* input))
  )

(defun print-out (out)
  (let ((r
	  (make-array (list (length out)) :initial-contents (mapcar #'code-char out) :element-type 'character)))

    (with-open-file (stream #p"/tmp/lol"
			    :direction :output
			    :if-exists :overwrite
			    :if-does-not-exist :create)
      (format stream r))

    ))

(defparameter *code-1*
  '("NOT A J"
    "NOT B T"
    "OR T J"
    "NOT C T"
    "OR T J"
    "AND D J"
    "WALK"))


(defparameter *code-2*
  '("NOT A J" ;; NOT A or NOT B or NOT C
    "NOT B T"
    "OR T J"
    "NOT C T"
    "OR T J"
    "AND D J" ;; AND D
    "NOT E T" ;;  AND ( E or H )
    "NOT T T"
    "OR H T"
    "AND T J"
    "RUN"))

(defun solution-1 ()
  (let ((*program* (allocate-program-memory (read-input) 3000)))
    (-> (run-droid-code *code-1*) (last) (car))))

(defun solution-2 ()
  (let ((*program* (allocate-program-memory (read-input) 3000)))
    (-> (run-droid-code *code-2*) (last) (car))))
