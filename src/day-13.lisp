(defpackage :day-13
  (:use :cl :advent-of-code :iterate :alexandria)
  (:import-from :day-5 :run-program-1 :allocate-program-memory)
  (:export :solution-1 :solution-2))

(in-package :day-13)

(defun read-input ()
  (read-code (resource-file #p"day-13-input.txt")))

(defun read-outputs (program number &optional (ip 0) inputs results)
  (if (zerop number)
      (values ip (nreverse results))
      (multiple-value-bind (next-ip result status next-inputs) (run-program-1 program inputs ip)
	(if (eq status :output)
	    (read-outputs program (1- number) next-ip next-inputs (cons result results))
	    (values next-ip (nreverse results))))))

(defun populate-board (board program &optional (ip 0))
  (multiple-value-bind (next-ip results) (read-outputs program 3 ip)
      (when (= 3 (length results))
	(setf (gethash (cons (car results) (cadr results)) board) (caddr results))
	(when next-ip
	  (populate-board board program next-ip)))))

(defparameter *block* 2)

(defun solution-1 ()
  (let* ((input (read-input))
	 (board (make-hash-table :test #'equal)))
    (populate-board board (allocate-program-memory input))
    (length (delete-if-not (curry #'eql *block*) (hash-table-values board)))))

(defun joystick (paddle ball)
  (cond ((< paddle ball) 1)
	((> paddle ball) -1)
	(t 0)))

(defun run-game (program)
  (iter
    (with ip = 0)
    (with paddle = 0)
    (with ball = 0)
    (with score = 0)
    (multiple-value-bind (next-ip results) (read-outputs program 3 ip (list (joystick paddle ball)))
      (while next-ip)
      (setq ip next-ip)
      (destructuring-bind (x y tile) results
	(when (and (= x -1) (zerop y))
	  (setq score (caddr results)))
	(case tile
	  (3 (setq paddle x))
	  (4 (setq ball x))))
      (finally (return score)))))

(defun solution-2 ()
  (let ((input (read-input)))
    (setf (svref input 0) 2)
    (run-game (allocate-program-memory input))))
