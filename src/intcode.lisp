(defpackage #:intcode
  (:use #:cl #:iterate #:aoc)
  (:import-from #:alexandria #:copy-array)
  (:export #:read-intcode #:file->program #:allocate-program #:decode #:run-program #:copy-code #:program-ip #:program-status #:program-code))

;; Intcode program interpreter

(in-package #:intcode)

(defstruct program
  (code nil :type (array integer))
  (ip 0 :type integer)
  (base 0 :type integer)
  (status :running :type keyword))

;; (defconstant +add+ 1)
;; (defconstant +multiply+ 2)
;; (defconstant +in+ 3)
;; (defconstant +out+ 4)
;; (defconstant +jmp-if-not-zero+ 5)
;; (defconstant +jmp-if-zero+ 6)
;; (defconstant +less-than+ 7)
;; (defconstant +equals+ 8)
;; (defconstant +adjust-base+ 9)
;; (defconstant +halt+ 99)

(defun get-params (program address number-of-read-params number-of-params &optional param-modes)
  (when (plusp number-of-params)
    (let* ((param-mode (car param-modes))
	   (param (aref (program-code program) (1+ address)))
	   (offset (case param-mode
                     (0 param)
		     (1 -1)
		     (2 (+ (program-base program) param))))
	   (value (cond
		    ((eql param-mode 1) param)
		    ((plusp number-of-read-params) (aref (program-code program) offset))
		    (t offset))))
      (cons value (get-params program (1+ address) (1- number-of-read-params) (1- number-of-params) (cdr param-modes))))))

(defun binary-op (fn program &optional param-modes)
  (destructuring-bind (op-1 op-2 result) (get-params program (program-ip program) 2 3 param-modes)
    (let ((value (funcall fn op-1 op-2)))
      (setf (aref (program-code program) result) value)
      (+ 4 (program-ip program)))))

(defun read-input (program read-fn &optional param-modes)
  (destructuring-bind (address) (get-params program (program-ip program) 0 1 param-modes)
    (let ((input (funcall read-fn)))
      (if input
	  (progn
	    (setf (aref (program-code program) address) input)
	    (+ 2 (program-ip program)))
	  (program-ip program)))))

(defun write-to-output (program write-fn &optional param-modes)
  (let ((params (get-params program (program-ip program) 1 1 param-modes)))
    (funcall write-fn (car params))
    (+ 2 (program-ip program))))

(defun param-modes (param-modes-number)
  (multiple-value-bind (v1 r1) (floor param-modes-number 10)
    (multiple-value-bind (v2 r2) (floor v1 10)
      (list r1 r2 (nth-value 1 (floor v2 10))))))

(defun decode (instr)
  (multiple-value-bind (param-modes-number opcode) (floor instr 100)
    (values opcode (param-modes param-modes-number))))

(defun cond-jmp (program pred &optional param-modes)
  (destructuring-bind (val-1 val-2) (get-params program (program-ip program) 2 2 param-modes)
    (if (funcall pred val-1)
	val-2
	(+ 3 (program-ip program)))))

(defun compare (program pred &optional param-modes)
  (destructuring-bind (val-1 val-2 address) (get-params program (program-ip program) 2 3 param-modes)
    (let ((result (if (funcall pred val-1 val-2) 1 0)))
      (setf (aref (program-code program) address) result)
      (+ 4 (program-ip program)))))

(defun adjust-base (program &optional param-modes)
  (destructuring-bind (val) (get-params program (program-ip program) 1 1 param-modes)
    (incf (program-base program) val)
    (+ 2 (program-ip program))))

(defun ->program-status (ip prev-ip)
  (cond
    ((null ip) :halt)
    ((eql prev-ip ip) :input)
    (t :running)))

(defun do-step (program read-fn write-fn)
  (multiple-value-bind (opcode param-modes) (decode (aref (program-code program) (program-ip program)))
    (let ((next-ip (ecase opcode
		     (1 (binary-op #'+ program param-modes))
		     (2 (binary-op #'* program param-modes))
		     (3 (read-input program read-fn param-modes))
		     (4 (write-to-output program write-fn param-modes))
		     (5 (cond-jmp program (complement #'zerop) param-modes))
		     (6 (cond-jmp program #'zerop param-modes))
		     (7 (compare program #'< param-modes))
		     (8 (compare program #'eql param-modes))
		     (9 (adjust-base program param-modes))
		     (99 nil))))
      (setf (program-status program) (->program-status next-ip (program-ip program)))
      (setf (program-ip program) (or next-ip -1)))))

;; runs intcode program until it halts , asks for input or max-outputs are generated
;; returns list of output values or a single value if max-outputs is 1
(defun run-program (program &key input (max-outputs 1 max-outputs-supplied-p))
  (iter
    (with result)
    (do-step program (λ () (pop input)) (λ (x) (push x result)))
    (while (and
	    (or
	     (null max-outputs-supplied-p)
	     (< (length result) max-outputs))
	    (eq (program-status program) :running)))
    (finally (return (let ((rev (nreverse result)))
		       (if (and
			    max-outputs-supplied-p
			    (= 1 max-outputs))
			   (car rev)
			   rev))))))

(defun allocate-program (code &optional (additional-memory-size 200))
  (make-program :code (adjust-array code (list (+ (length code) additional-memory-size)) :fill-pointer t)))

(defun read-intcode (file)
  (let* ((input (read-string #'parse-integer file)))
    (make-array (list (length input)) :initial-contents input :element-type 'integer :adjustable t :fill-pointer t)))

(defun file->program (file &optional (additional-memory-size 200))
  (-> file
    read-intcode
    (allocate-program additional-memory-size)))

(defun copy-code (program)
  (let ((copy (copy-program program)))
    (setf (program-code program) (copy-array (program-code program)))
    copy))
