(defpackage :day-2
  (:use :cl :iterate :advent-of-code)
  (:export :solution-1 :run-intcode :solution-2))

(in-package :day-2)

(defparameter *add* 1)
(defparameter *multiply* 2)
(defparameter *halt* 99)

(defun binary-op (fn ip code)
  (let* ((op-1 (svref code (1+ ip)))
	 (op-2 (svref code (+ 2 ip)))
	 (result (svref code (+ 3 ip)))
	 (value (funcall fn (svref code op-1) (svref code op-2))))
    (setf (svref code result) value)
    (+ 4 ip)))

(defun do-step (ip code)
  (let ((opcode (svref code ip)))
    (cond
      ((eql opcode *add*) (binary-op #'+ ip code))
      ((eql opcode *multiply*) (binary-op #'* ip code))
      ((eql opcode *halt*) nil)
      (t (error "Invalid opcode")))))

(defun run-intcode (code)
  (iterate
    (initially (setq ip 0))
    (for ip next (do-step ip code))
    (while ip)
    (finally (return code))))

(defun solution-1 ()
  (let ((code (read-code #p"day-2-input.txt")))
    (setf (svref code 1) 12)
    (setf (svref code 2) 2)
    (svref (run-intcode code) 0)))

(defun try-input (code arg1 arg2)
  (setf (svref code 1) arg1)
  (setf (svref code 2) arg2)
  (svref (run-intcode (copy-seq code)) 0))

(defun solution-2 ()
  (let* ((code (read-code #p"day-2-input.txt"))
	 (size (length code))
	 (args (iterate outer
		 (for i from 0 below size)
		 (iterate (for j from 0 below size)
		   (when (eql 19690720 (try-input code i j))
		     (return-from outer (cons i j)))))))
    (+ (* 100 (car args)) (cdr args))))
