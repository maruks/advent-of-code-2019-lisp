(defpackage :day-3
  (:use :cl :iterate :split-sequence :advent-of-code)
  (:export :solution-1 :->points :intersect :closest-intersection))

(in-package :day-3)

(defparameter *central-point* (make-point :x 0 :y 0))

(defun read-input ()
  (let* ((lines (read-file "day-3-input.txt"))
	 (line-1 (parse-line (car lines)))
	 (line-2 (parse-line (cadr lines))))
    (cons line-1 line-2)))

(defun parse-move (string)
  (cons (intern (subseq string 0 1) "KEYWORD") (parse-integer (subseq string 1))))

(defun parse-line (line)
  (mapcar #'parse-move (split-sequence #\, line)))

(defun direction (point-1 point-2)
  (if (eql (point-x point-1) (point-x point-2))
      :vertical
      :horizontal))

(defun move-point (dir dist point)
  (let ((p (copy-structure point)))
    (ecase dir
      (:D (setf (point-y p) (+ (point-y p) dist)))
      (:U (setf (point-y p) (- (point-y p) dist)))
      (:R (setf (point-x p) (+ (point-x p) dist)))
      (:L (setf (point-x p) (- (point-x p) dist))))
    p))

(defun ->points (moves)
  (labels ((->pts (result moves prev-point)
	     (if moves
		 (let* ((head (car moves))
			(moved (move-point (car head) (cdr head) prev-point)))
		   (->pts (cons prev-point result) (cdr moves) moved))
		 (nreverse (cons prev-point result)))))
    (->pts nil moves *central-point*)))

(defun closest-intersection (moves-1 moves-2)
  (let ((points-1 (->points moves-1))
	(points-2 (->points moves-2)))
    (iterate outer
      (for p1b :in points-1)
      (for p1a :previous p1b)
      (when (first-iteration-p)
	(next-iteration))
      (iterate
	(for p2b :in points-2)
	(for p2a :previous p2b)
	(when (first-iteration-p)
	  (next-iteration))
	(for i = (intersect p1a p1b p2a p2b))
	(when (or (null i) (equalp i (cons 0 0)))
	  (next-iteration))
	(in outer (minimize (distance *central-point* (make-point :x (car i) :y (cdr i)))))))))


(defparameter m1   	    '((:R . 8) (:U . 5) (:L . 5) (:D . 3) ) )
(defparameter m2   	    '((:U . 7) (:R . 6) (:D . 4) (:L . 4)) )

(defun lowest-steps (moves-1 moves-2)
  (let ((points-1 (->points moves-1))
	(points-2 (->points moves-2)))
    (iterate outer
      (for p1b :in points-1)
      (for p1a :previous p1b)
      (when (first-iteration-p)
	(next-iteration))
      (iterate
	(for p2b :in points-2)
	(for p2a :previous p2b)
	(when (first-iteration-p)
	  (next-iteration))
	(for i = (intersect p1a p1b p2a p2b))
	(when (or (null i) (equalp i (cons 0 0)))
	  (next-iteration))
	(in outer (minimize (distance *central-point* (make-point :x (car i) :y (cdr i)))))))))

(defun solution-1 ()
  (let* ((input (read-input))
	 (moves-1 (car input))
	 (moves-2 (cdr input)))
    (closest-intersection moves-1 moves-2)))

(defun intersect-point (hp1 hp2 vp1 vp2)
  (flet ((between (p1 p2 p3) (and (<= p1 p2) (<= p2 p3))))
    (let ((vx1 (point-x vp1))
	  (vy1 (point-y vp1))
	  (vy2 (point-y vp2))
  	  (hx1 (point-x hp1))
	  (hx2 (point-x hp2))
	  (hy1 (point-y hp1)))
      (when (and (between vy1 hy1 vy2)
		 (between hx1 vx1 hx2))
	(cons vx1 hy1)))))

(defun intersect (point-1 point-2 point-3 point-4)
  (let ((dir-1-2 (direction point-1 point-2))
	(dir-3-4 (direction point-3 point-4)))
    (when (not (eq dir-1-2 dir-3-4))
      (destructuring-bind (p1 p2) (sort (list point-1 point-2) #'compare-points)
	(destructuring-bind (p3 p4) (sort (list point-3 point-4) #'compare-points)
	  (if (eq :horizontal dir-1-2)
	      (intersect-point p1 p2 p3 p4)
	      (intersect-point p3 p4 p1 p2)))))))
