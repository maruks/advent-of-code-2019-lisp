(defpackage :day-10
  (:use :cl :advent-of-code :iterate :alexandria)
  (:export :read-map :visible-points :best-location-point :solution-1 :solution-2))

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

(defun find-hidden (p1 p2 points)
  (remove-if-not (curry #'is-hidden p1 p2) points))

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

(defun atan-degrees (x)
  (float (* (/ 180 pi) (atan x))))

(defun angle (dx dy)
  (cond
    ((zerop dx) (if (plusp dy) 90.0 -90.0))
    ((plusp dx) (atan-degrees (float (/ dy dx))))
    ((minusp dx) (- -180.0 (atan-degrees (- (float (/ dy dx))))) )))

(defun find-next-point (from points &optional (prev-angle 90.0))
  (let ((x (point-x from))
	(y (point-y from)))
    (iter
      (for p :in points)
      (for angle = (angle (- (point-x p) x) (- y (point-y p) )))
      (finding (list p angle) minimizing (- prev-angle angle)))))

(defun collect-points (from points &optional (angle 90.0) result)
  (flet ((sort-by-distance (p1 p2) (< (dist from p1) (dist from p2))))
    (if points
	(destructuring-bind (point next-angle) (find-next-point from points angle)
	  (collect-points
	   from
	   (remove-hidden from point points)
	   next-angle
	   (cons (sort (find-hidden from point points) #'sort-by-distance) result)))
	(make-array (list (length result)) :initial-contents (nreverse result)))))

(defun vaporized (points vec-size nth &optional (index 0) prev-point)
  (if (zerop nth)
      prev-point
      (if-let (point (pop (svref points (mod index vec-size))))
	(vaporized points vec-size (1- nth) (1+ index) point)
	(vaporized points vec-size nth (1+ index)))))

(defun vaporized-point (points nth)
  (let* ((station-point (cadr (best-location-point points)))
	 (pts (collect-points station-point (remove station-point points :test #'equalp))))
    (vaporized pts (length pts) nth)))

(defun solution-2 ()
  (vaporized-point (read-map (read-input)) 200))
