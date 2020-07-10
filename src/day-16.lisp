(defpackage #:day-16
  (:use #:cl #:aoc #:iterate #:alexandria)
  (:export #:solution-1 #:solution-2 #:pattern-number #:process-all))

(in-package #:day-16)

(defun read-input ()
  (mapcar (lambda (d) (- (char-code d) 48))
	  (car (read-lines (resource-file #p"day-16-input.txt") (rcurry #'coerce 'list)))))

(defparameter *base-pattern* '(0 1 0 -1))

(defun pattern-number (position index)
  (nth
   (rem (truncate (1+ index) (1+ position)) (length *base-pattern*))
   *base-pattern*))

(defun fft (position prev-list &optional (index 0) (sum 0))
  (if-let (n (car prev-list))
    (fft position (cdr prev-list) (1+ index) (+ sum (* n (pattern-number position index))))
    (rem (abs sum) 10)))

(defun process-all (input iterations)
  (iter
    (with size = (length input))
    (for i :below iterations)
    (for xs :initially input :then (mapcar (rcurry #'fft xs) (iota size)))
    (finally (return xs))))

(defun solution-1 ()
  (let ((result (process-all (read-input) 100)))
    (digits->number (subseq result 0 8))))

(defun digits->number (xs &optional (result 0))
  (if-let (d (car xs))
    (digits->number (cdr xs) (+ d (* 10 result)))
    result))

(defun append-n-times (xs n &optional result)
  (if (zerop n)
      result
      (append-n-times xs (1- n) (append xs result))))

(defun fft-2 (prev-list index result)
  (if (zerop index)
      result
      (fft-2 (cdr prev-list) (1- index) (cons (rem (+ (car result) (car prev-list)) 10) result))))

(defun all-iterations (input n last-elem-list)
  (iter
    (for i :below n)
    (for xs :initially input :then (fft-2 (cdr (reverse xs)) (1- (length input)) last-elem-list))
    (finally (return xs))))

(defun solution-2 ()
  (let* ((input (read-input))
	 (size (length input))
	 (offset (digits->number (subseq input 0 7)))
	 (seq-size (- (* 10000 size) offset))
	 (copy-vals (multiple-value-list (ceiling seq-size size)))
	 (copy-times (car copy-vals))
	 (copy-offset (cadr copy-vals))
	 (last-elem-list (last input))
	 (copied (append-n-times input copy-times))
	 (seq (subseq copied (- copy-offset) (length copied)))
	 (result (all-iterations seq 100 last-elem-list)))
    (digits->number (subseq result 0 8))))
