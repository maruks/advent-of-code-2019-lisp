(defpackage #:day-6
  (:use #:cl #:split-sequence #:alexandria #:iterate #:aoc)
  (:export #:solution-1 #:solution-2))

(in-package #:day-6)

(defun read-input (file)
  (flet ((readln (str)
	   (mapcar #'make-keyword (split-sequence #\) str))))
    (let* ((input (read-lines file #'readln))
	   (size (length input))
	   (result (make-hash-table :test #'eq :size size)))
      (iter
	(for (k v) :in input)
	(let ((vals (gethash k result)))
	  (setf (gethash k result) (if (null vals) (list v) (cons v vals))))
	(finally (return result))))))

(defvar *graph*)

(defun count-orbits (node &optional (orbits 0))
  (let ((results (mapcar (rcurry #'count-orbits (1+ orbits)) (gethash node *graph*))))
    (reduce #'+ results :initial-value orbits)))

(defun solution-1 ()
  (let ((*graph* (read-input #p"day-6-input.txt")))
    (count-orbits :COM)))

(defun path-to (from to &optional visited)
  (let ((children (gethash from *graph*)))
    (if (eq from to)
	(nreverse visited)
	(let ((results (mapcar (rcurry #'path-to to (cons from visited)) children)))
	  (find-if-not #'null results)))))


(defun orbital-distance (path-1 path-2)
  (if (eq (car path-1) (car path-2))
      (orbital-distance (cdr path-1) (cdr path-2))
      (+ (length path-1) (length path-2))))

(defun solution-2 ()
  (let ((*graph* (read-input #p"day-6-input.txt")))
    (orbital-distance (path-to :COM :SAN) (path-to :COM :YOU))))
