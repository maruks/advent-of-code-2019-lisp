(defpackage :day-13
  (:use :cl :advent-of-code :alexandria)
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


(defvar *score*)
(defvar *paddle*)
(defvar *ball*)
(defvar *blocks*)
(defvar *game-started*)
(defvar *stop*)
(defvar *board*)
(defvar *ip*)

(defun joystick ()
  (cond ((< *paddle* *ball*) 1)
	((> *paddle* *ball*) -1)
	(t 0)))

(defun run-game (program &optional (ip 0))

  (if (null *stop*)

      (multiple-value-bind (next-ip results) (read-outputs program 3 ip (list (joystick)))

	(format t " ~a ~a ~%" results next-ip )

      (when (= 3 (length results))


	(setq *ip* next-ip)

	(destructuring-bind (x y tile) results

	  (when (and (= x -1) (zerop y))
	    (setq *score* (caddr results))
	    (when (zerop *block*)
	      (format t "score  ~a  ~%"  *score*)
	      (setq *stop* t)))

	  (case tile
	    (0 (when (remhash (cons x y) *board*)
		 (decf *blocks*)))
	    (2 (setf (gethash (cons x y) *board*) *block*))
	    (3 (setq *paddle* x))
	    (4 (progn (when (null *game-started*)
			(setq *blocks* (hash-table-count *board*))
			(setq *game-started* t))
		      (setq *ball* x))))

	  (format t "next ~a  ~%" next-ip )
	  (when next-ip (run-game program next-ip))

	  )))


    (progn
      (format t "score  ~a  ~%"  *score*)
      *score*)

    )

  )

(defun solution-2 ()
  (let* ((input (read-input))
	 (*blocks* 0)
	 (*score* 0)
	 (*paddle* 0)
	 (*game-started* nil)
	 (*stop* nil)
	 (*ball* 0)
	 (*board* (make-hash-table :test #'equal)))
    (setf (svref input 0) 2)
    (run-game (allocate-program-memory input))
    *score*))
