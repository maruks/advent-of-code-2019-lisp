(defpackage #:day-13
  (:use #:cl #:aoc #:iterate #:alexandria)
  (:import-from #:intcode #:file->program #:run-program #:read-intcode #:allocate-program #:program-status #:program-code)
  (:export #:solution-1 #:solution-2))

(in-package #:day-13)

(defun populate-board (board outputs)
  (when outputs
    (setf (gethash (cons (car outputs) (cadr outputs)) board) (caddr outputs))
    (populate-board board (cdddr outputs))))

(define-constant +block+ 2)

(defun read-input ()
  (file->program #p"day-13-input.txt"))

(defun solution-1 ()
  (let* ((input (read-input))
	 (board (make-hash-table :test #'equal))
	 (outputs (run-program input)))
    (populate-board board outputs)
    (length (delete-if-not (curry #'eql +block+) (hash-table-values board)))))

(defun joystick (paddle ball)
  (cond ((< paddle ball) 1)
	((> paddle ball) -1)
	(t 0)))

(defun run-game (program)
  (iter
    (with paddle = 0)
    (with ball = 0)
    (with score = 0)
    (let ((results (run-program program :max-outputs 3 :input (list (joystick paddle ball)))))
      (until (eq :halt (program-status program)))
      (destructuring-bind (x y tile) results
	(when (and (= x -1) (zerop y))
	  (setq score tile))
	(case tile
	  (3 (setq paddle x))
	  (4 (setq ball x))))
      (finally (return score)))))

(defun solution-2 ()
  (let ((input (read-input)))
    (setf (aref (program-code input) 0) 2)
    (run-game input)))
