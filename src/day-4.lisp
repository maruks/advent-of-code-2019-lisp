(defpackage :day-4
  (:use :cl :aoc)
  (:export :solution-1 :solution-2))

(in-package :day-4)

(defun test-number (number &optional prev-digit has-double?)
  (if (plusp number)
      (multiple-value-bind (next-num rem) (floor number 10)
      (and (<= rem (or prev-digit 9))
	   (test-number next-num rem (or has-double? (eql rem prev-digit)))))
    has-double?))

(defun count-numbers (num max-num sum)
  (if (> num max-num)
      sum
      (count-numbers (1+ num) max-num (if (test-number num) (1+ sum) sum))))

(defun solution-1 ()
  (count-numbers 236491 713787 0))

(defun test-number-2 (number &optional prev-digit (num-of-matches 0) has-double?)
  (if (plusp number)
      (multiple-value-bind (next-num rem) (floor number 10)
	(when (<= rem (or prev-digit 9))
	  (let* ((is-match? (eql rem prev-digit))
		 (next-num-of-matches (if is-match? (1+ num-of-matches) 0))
		 (found-double? (and (eql num-of-matches 1) (not is-match?))))
	    (test-number-2 next-num rem next-num-of-matches (or has-double? found-double?)))))
      (or has-double? (eql num-of-matches 1))))

(defun count-numbers-2 (num max-num sum)
  (if (> num max-num)
      sum
      (count-numbers-2 (1+ num) max-num (if (test-number-2 num) (1+ sum) sum))))

(defun solution-2 ()
  (count-numbers-2 236491 713787 0))
