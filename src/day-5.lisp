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

(defun binary-op (fn ip program &optional param-modes)
  (destructuring-bind (p1 p2 _) param-modes
    (declare (ignore _))
    (let* ((op-1 (svref program (1+ ip)))
	   (op-2 (svref program (+ 2 ip)))
	   (result (svref program (+ 3 ip)))
	   (value (funcall fn (if (eql p1 1) op-1 (svref program op-1)) (if (eql p2 1) op-2 (svref program op-2)))))
      (setf (svref program result) value)
      (+ 4 ip))))

(defun read-input (ip program)
  (let ((address (svref program (1+ ip))))
    (setf (svref program address) *input*)
    (+ 2 ip)))

(defun write-to-output (ip program &optional param-modes)
  (let ((address (svref program (1+ ip)))
	(p1 (car param-modes)))
    (setq *output* (if (eql 1 p1) address (svref program address)))
    (+ 2 ip)))

(defun param-modes (param-modes-number)
  (multiple-value-bind (v1 r1) (floor param-modes-number 10)
    (multiple-value-bind (v2 r2) (floor v1 10)
      (list r1 r2 (nth-value 1 (floor v2 10))))))

(defun decode (instr)
  (multiple-value-bind (param-modes-number opcode) (floor instr 100)
    (values opcode (param-modes param-modes-number))))

(defun cond-jmp (ip program pred &optional param-modes)
    (destructuring-bind (p1 p2 _) param-modes
    (declare (ignore _))
    (let* ((op-1 (svref program (1+ ip)))
	   (op-2 (svref program (+ 2 ip)))
           (val-1 (if (eql p1 1) op-1 (svref program op-1)))
	   (val-2 (if (eql p2 1) op-2 (svref program op-2))))
      (if (funcall pred val-1)
	  val-2
	  (+ 3 ip)))))

(defun compare (ip program pred &optional param-modes)
  (destructuring-bind (p1 p2 _) param-modes
    (declare (ignore _))
    (let* ((op-1 (svref program (1+ ip)))
	   (op-2 (svref program (+ 2 ip)))
	   (address (svref program (+ 3 ip)))
           (val-1 (if (eql p1 1) op-1 (svref program op-1)))
	   (val-2 (if (eql p2 1) op-2 (svref program op-2)))
	   (result (if (funcall pred val-1 val-2) 1 0)))
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
