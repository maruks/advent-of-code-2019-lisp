(defpackage :day-10
  (:use :cl :advent-of-code :iterate :alexandria)
  (:export :read-map :visible-points :best-location-point :solution-1)
  )

(in-package :day-10)

(defun read-map (lines)
  (iter outer
    (for i :in lines)
    (for y :from 0)
    (iter
      (for x :below (length i))
      (when (char= #\# (schar i x))
	(in outer (collect (make-point :x x :y y)))))))

(defun read-input ()
  (read-lines #'identity (resource-file #p"day-10-input.txt")))

(defun dist (point-1 point-2)
  (sqrt (+ (expt (- (point-x point-1) (point-x point-2)) 2)
	   (expt (- (point-y point-1) (point-y point-2)) 2))))

(defun float-eq (a b)
  (< (abs (- a b)) 0.00001))

(defun is-hidden (p0 p1 p2)
  (or
   (float-eq (+ (dist p0 p1) (dist p1 p2)) (dist p0 p2))
   (float-eq (+ (dist p0 p2) (dist p2 p1)) (dist p0 p1))))

(defun remove-hidden (p1 p2 points)
  (remove-if (curry #'is-hidden p1 p2) points))

(defun visible-points (point-a points &optional result)
  (if result
      (if-let (point-b (car points))
	(visible-points point-a (remove-hidden point-a point-b points) (1+ result))
	result)
      (visible-points point-a (remove point-a points :test #'equalp) 0)))

(defun best-location-point (points)
  (iter
    (for p :in points)
    (for v = (visible-points p points))
    (finding (list v p) maximizing v)))

(defun max-visible-points (points)
  (car (best-location-point points)))

(defun solution-1 ()
  (let ((map (read-map (read-input))))
    (max-visible-points map)))
