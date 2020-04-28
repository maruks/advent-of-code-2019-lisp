(defpackage :day-17
  (:use :cl :advent-of-code :iterate :alexandria)
  (:import-from :day-5 :run-program-1 :run-program-collect-results :allocate-program-memory)
  (:export :solution-1 :solution-2))

(in-package :day-17)

(defun read-input ()
  (read-code (resource-file #p"day-17-input.txt")))

(defun populate-map (xs map &optional (x 0) (y 0))
  (when-let (code (car xs))
    (let ((char (code-char code)))
      (if (char= #\Newline char)
	  (populate-map (cdr xs) map 0 (1+ y))
	  (progn
	    (setf (gethash (cons x y) map) char)
	    (populate-map (cdr xs) map (1+ x) y))))))

(defun print-map (map)
  (iter
    (for y :from 50 :downto 0)
    (iter
      (for x :below 50)
      (format t "~a" (gethash (cons x (- 50 y) ) map #\Space)))
    (format t "~%")))

(defun adjacent (point)
  (let ((x (car point))
	(y (cdr point)))
    (list (cons (1+ x) y) (cons (1- x) y) (cons x (1+ y)) (cons x (1- y)))))

(defun locate-intersections (map)
  (iter
    (for (k v) :in-hashtable map)
    (when (and
	   (char= v #\#)
	   (every (curry #'char= #\#) (mapcar (rcurry #'gethash map #\G) (adjacent k))))
      (collect k))))

(defun solution-1 ()
  (let* ((input (allocate-program-memory (read-input)))
	 (map (make-hash-table :test #'equal))
	 (out (run-program-collect-results input '())))
    (populate-map out map)
    (reduce #'+ (mapcar (lambda (p) (* (car p) (cdr p))) (locate-intersections map)) :initial-value 0)
    )
  )

(defun solution-2 ()
  )
