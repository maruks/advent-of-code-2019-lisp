(defpackage #:day-21
  (:use #:cl #:aoc #:iterate #:alexandria)
  (:import-from #:intcode #:file->program #:run-program)
  (:import-from #:str #:concat #:add-suffix)
  (:export #:solution-1 #:solution-2))

(in-package #:day-21)

(defun read-input ()
  (file->program #p"day-21-input.txt"))

(defun run-droid-code (program code)
  (let ((input (->> (coerce (apply #'concat (add-suffix code (string #\Newline))) 'list)
		 (mapcar #'char-code))))
    (run-program program :input input)))

(define-constant +code-1+
    '("NOT A J"
      "NOT B T"
      "OR T J"
      "NOT C T"
      "OR T J"
      "AND D J"
      "WALK")
  :test #'equal)


(define-constant +code-2+
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
      "RUN")
  :test #'equal)

(defun solution-1 ()
  (-> (read-input)
    (run-droid-code +code-1+)
    last
    car))

(defun solution-2 ()
  (-> (read-input)
    (run-droid-code +code-2+)
    last
    car))
