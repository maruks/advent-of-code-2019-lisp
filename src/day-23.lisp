(defpackage #:day-23
  (:use #:cl #:aoc #:iterate #:alexandria #:queues)
  (:import-from #:intcode #:file->program #:run-program #:copy-code #:program-status)
  (:export #:solution-1 #:solution-2))

(in-package #:day-23)

(defparameter *nat* nil)
(defparameter *prev-nat-y* nil)
(defparameter *computers* nil)
(defparameter *queues* nil)

(define-constant +number-of-computers+ 50)
(define-constant +nat-address+ 255)

(defun read-input ()
  (file->program #p"day-23-input.txt"))

(defun solution (run-fn)
  (let ((code (read-input))
	(*nat* nil)
	(*prev-nat-y* nil)
	(*computers* (make-array +number-of-computers+))
	(*queues* (make-array +number-of-computers+)))
    (iter
      (for i :below +number-of-computers+)
      (setf (svref *computers* i) (copy-code code))
      (let ((io-queue (make-queue :simple-queue)))
	(setf (svref *queues* i) io-queue)
	(qpush io-queue i)))
    (funcall run-fn)))

(defun solution-1 ()
  (cdr (solution #'run-computers-1)))

(defun run-computer (idx &optional is-input? outputs)
  (let* ((program (svref *computers* idx))
	 (queue (svref *queues* idx))
	 (input (when is-input?
		  (qpop queue)))
	 (inputs (when is-input?
		   (list (or input -1))))
	 (result (run-program program :input inputs :max-outputs 1)))
    (ecase (program-status program)
      (:running (run-computer idx nil (cons result outputs)))
      (:input (if (and is-input? (null input))
		  (nreverse outputs)
		  (run-computer idx t outputs))))))

(defun run-computers-1 ()
  (iter
    (for i :below +number-of-computers+)
    (-> (run-computer i)
      (dispatch-packets)))
  (or *nat* (run-computers-1)))

(defun send-nat (packets-sent)
  (when (and (zerop packets-sent) *nat*)
    (destructuring-bind (x . y) *nat*
      (qpush (svref *queues* 0) x)
      (qpush (svref *queues* 0) y)
      (prog1 (when (eql *prev-nat-y* y) y)
	(setq *prev-nat-y* y)))))

(defun dispatch-packets (packets)
  (when packets
    (destructuring-bind (address x y &rest rest) packets
      (if (eql address +nat-address+)
	  (setq *nat* (cons x y))
	  (progn
	    (qpush (svref *queues* address) x)
	    (qpush (svref *queues* address) y)))
      (dispatch-packets rest))))

(defun run-computers-2 ()
  (let ((packets-sent 0))
    (iter
      (for i :below +number-of-computers+)
      (let ((packets (run-computer i)))
	(dispatch-packets packets)
	(incf packets-sent (length packets))))
    (or (send-nat packets-sent) (run-computers-2))))

(defun solution-2 ()
  (solution #'run-computers-2))
