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

(defun count-orbits (node graph &optional (orbits 0))
  (let ((results (mapcar (rcurry #'count-orbits graph (1+ orbits)) (gethash node graph))))
    (reduce #'+ results :initial-value orbits)))

(defun solution-1 ()
  (->> #p"day-6-input.txt"
    read-input
    (count-orbits :COM)))

(defun path-to (from to graph &optional visited)
  (let ((children (gethash from graph)))
    (if (eq from to)
	(nreverse visited)
	(let ((results (mapcar (rcurry #'path-to to graph (cons from visited)) children)))
	  (find-if-not #'null results)))))

(defun orbital-distance (path-1 path-2)
  (if (eq (car path-1) (car path-2))
      (orbital-distance (cdr path-1) (cdr path-2))
      (+ (length path-1) (length path-2))))

(defun solution-2 ()
  (let ((graph (read-input #p"day-6-input.txt")))
    (orbital-distance (path-to :COM :SAN graph) (path-to :COM :YOU graph))))
