(defpackage :day-5
  (:use :cl :iterate :advent-of-code)
  (:export :solution-1 :run-intcode :solution-2 :decode :run-program))

(in-package :day-5)

(defun read-program (file)
  (let* ((input (read-string #'parse-integer file)))
    (apply #'vector input)))

(defparameter *add* 1)
(defparameter *multiply* 2)
(defparameter *in* 3)
(defparameter *out* 4)
(defparameter *jmp-if-not-zero* 5)
(defparameter *jmp-if-zero* 6)
(defparameter *less-than* 7)
(defparameter *equals* 8)
(defparameter *halt* 99)

(defvar *input*)
(defvar *output*)

(defun get-params (ip program number-of-read-params number-of-params &optional param-modes)
  (when (plusp number-of-params)
    (let* ((param-mode-1 (or (eql (car param-modes) 1) (not (plusp number-of-read-params))))
	   (param (svref program (1+ ip)))
	   (value (if param-mode-1 param (svref program param))))
      (cons value (get-params (1+ ip) program (1- number-of-read-params) (1- number-of-params) (cdr param-modes))))))

(defun binary-op (fn ip program &optional param-modes)
  (destructuring-bind (op-1 op-2 result) (get-params ip program 2 3 param-modes)
    (let ((value (funcall fn op-1 op-2)))
      (setf (svref program result) value)
      (+ 4 ip))))

(defun read-input (ip program)
  (let ((address (svref program (1+ ip))))
    (setf (svref program address) *input*)
    (+ 2 ip)))

(defun write-to-output (ip program &optional param-modes)
  (let ((params (get-params ip program 1 1 param-modes)))
    (setq *output* (car params))
    (+ 2 ip)))

(defun param-modes (param-modes-number)
  (multiple-value-bind (v1 r1) (floor param-modes-number 10)
    (multiple-value-bind (v2 r2) (floor v1 10)
      (list r1 r2 (nth-value 1 (floor v2 10))))))

(defun decode (instr)
  (multiple-value-bind (param-modes-number opcode) (floor instr 100)
    (values opcode (param-modes param-modes-number))))

(defun cond-jmp (ip program pred &optional param-modes)
  (destructuring-bind (val-1 val-2) (get-params ip program 2 2 param-modes)
    (if (funcall pred val-1)
	val-2
	(+ 3 ip))))

(defun compare (ip program pred &optional param-modes)
  (destructuring-bind (val-1 val-2 address) (get-params ip program 2 3 param-modes)
    (let ((result (if (funcall pred val-1 val-2) 1 0)))
      (setf (svref program address) result)
      (+ 4 ip))))

(defun do-step (ip program)
  (multiple-value-bind (opcode param-modes) (decode (svref program ip))
    (cond
      ((eql opcode *add*) (binary-op #'+ ip program param-modes))
      ((eql opcode *multiply*) (binary-op #'* ip program param-modes))
      ((eql opcode *in*) (read-input ip program))
      ((eql opcode *out*) (write-to-output ip program param-modes))
      ((eql opcode *jmp-if-zero*) (cond-jmp ip program #'zerop param-modes))
      ((eql opcode *jmp-if-not-zero*) (cond-jmp ip program (lambda (x) (not (zerop x))) param-modes))
      ((eql opcode *less-than*) (compare ip program #'< param-modes))
      ((eql opcode *equals*) (compare ip program #'eql param-modes))
      ((eql opcode *halt*) nil)
      (t (error "Invalid opcode")))))

(defun run-program (program input)
  (let ((*input* input))
    (iterate
      (initially (setq ip 0))
      (for ip next (do-step ip program))
      (while ip)
      (finally (return *output*)))))

(defun solution-1 ()
  (run-program (read-program #p"day-5-input.txt") 1))

(defun solution-2 ()
  (run-program (read-program #p"day-5-input.txt") 5))
