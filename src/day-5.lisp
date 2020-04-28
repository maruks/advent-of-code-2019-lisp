(defpackage :day-5
  (:use :cl :iterate :advent-of-code)
  (:export :solution-1 :solution-2 :decode :run-program :run-program-1 :allocate-program-memory :run-program-collect-results))

(in-package :day-5)

(defparameter *add* 1)
(defparameter *multiply* 2)
(defparameter *in* 3)
(defparameter *out* 4)
(defparameter *jmp-if-not-zero* 5)
(defparameter *jmp-if-zero* 6)
(defparameter *less-than* 7)
(defparameter *equals* 8)
(defparameter *adjust-base* 9)
(defparameter *halt* 99)

(defparameter *program-memory-size* 4000)

(defun get-params (ip program number-of-read-params number-of-params &optional param-modes)
  (when (plusp number-of-params)
    (let* ((param-mode (car param-modes))
	   (param (svref program (1+ ip)))
	   (offset (case param-mode
                     (0 param)
		     (1 -1)
		     (2 (+ (svref program (1- *program-memory-size*)) param))))
	   (value (cond
		    ((eql param-mode 1) param)
		    ((plusp number-of-read-params) (svref program offset))
		    (t offset))))
      (cons value (get-params (1+ ip) program (1- number-of-read-params) (1- number-of-params) (cdr param-modes))))))

(defun binary-op (fn ip program &optional param-modes)
  (destructuring-bind (op-1 op-2 result) (get-params ip program 2 3 param-modes)
    (let ((value (funcall fn op-1 op-2)))
      (setf (svref program result) value)
      (+ 4 ip))))

(defun read-input (ip program read-fn &optional param-modes)
  (destructuring-bind (address) (get-params ip program 0 1 param-modes)
    (let ((input (funcall read-fn)))
      (if input
	  (progn
	    (setf (svref program address) input)
	    (+ 2 ip))
	  ip))))

(defun write-to-output (ip program write-fn &optional param-modes)
  (let ((params (get-params ip program 1 1 param-modes)))
    (funcall write-fn (car params))
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

(defun adjust-base (ip program &optional param-modes)
  (destructuring-bind (val) (get-params ip program 1 1 param-modes)
    (let ((base-pointer (1- *program-memory-size*)))
      (incf (svref program base-pointer) val)
      (+ 2 ip))))

(defun do-step (ip program read-fn write-fn)
  (multiple-value-bind (opcode param-modes) (decode (svref program ip))
    (cond
      ((eql opcode *add*) (binary-op #'+ ip program param-modes))
      ((eql opcode *multiply*) (binary-op #'* ip program param-modes))
      ((eql opcode *in*) (read-input ip program read-fn param-modes))
      ((eql opcode *out*) (write-to-output ip program write-fn param-modes))
      ((eql opcode *jmp-if-zero*) (cond-jmp ip program #'zerop param-modes))
      ((eql opcode *jmp-if-not-zero*) (cond-jmp ip program (λ (x) (not (zerop x))) param-modes))
      ((eql opcode *less-than*) (compare ip program #'< param-modes))
      ((eql opcode *equals*) (compare ip program #'eql param-modes))
      ((eql opcode *adjust-base*) (adjust-base ip program param-modes))
      ((eql opcode *halt*) nil)
      (t (error "Invalid opcode")))))

(defun run-program (program inputs)
  (iter
    (initially (setq ip 0))
    (with result)
    (for ip next (do-step ip program (λ () (pop inputs)) (λ (x) (setq result x))))
    (while ip)
    (finally (return result))))

(defun program-status (ip prev-ip result)
  (cond
    ((null ip) :halt)
    ((not (null result)) :output)
    ((eql prev-ip ip) :input)
    (t :error)))

(defun run-program-1 (program inputs &optional (start-ip 0))
  (iter
    (initially (setq ip start-ip))
    (with result)
    (for ip :next (do-step ip program (λ () (pop inputs)) (λ (x) (setq result x))))
    (for prev-ip :previous ip :initially -1)
    (while (and ip (null result) (/= ip prev-ip)))
    (finally (return (values ip result (program-status ip prev-ip result) inputs)))))

(defun run-program-collect-results (program program-inputs)
  (iter
    (with ip = 0)
    (with inputs = program-inputs)
    (multiple-value-bind (next-ip result next-inputs) (run-program-1 program inputs ip)
      (while next-ip)
      (collect result)
      (setq ip next-ip)
      (setq inputs next-inputs))))

(defun solution-1 ()
  (run-program (read-code #p"day-5-input.txt") '(1)))

(defun solution-2 ()
  (run-program (read-code #p"day-5-input.txt") '(5)))

(defun allocate-program-memory (program)
  (let ((result (make-array (list *program-memory-size*) :initial-element 0)))
    (iter
      (for i :below (length program))
      (setf (svref result i) (svref program i))
      (finally (return result)))))
