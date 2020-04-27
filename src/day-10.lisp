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

(defun float-eq (a b)
  (< (abs (- a b)) 0.00001))

(defun atan-degrees (x)
  (float (* (/ 180 pi) (atan x))))

(defun angle (dx dy)
  (cond
    ((zerop dx) (if (plusp dy) 90.0 -90.0))
    ((plusp dx) (atan-degrees (float (/ dy dx))))
    ((minusp dx) (- -180.0 (atan-degrees (- (float (/ dy dx))))) )))

(defun angle-between-points (point-1 point-2)
  (let ((dx (- (point-x point-2) (point-x point-1)))
	(dy (- (point-y point-1) (point-y point-2))))
    (angle dx dy)))

(defun all-angles (from points)
  (let* ((angles (mapcar (curry #'angle-between-points from) points)))
    (sort angles #'<)))

(defun unique-angles (angles prev-angle unique)
  (if-let (angle (car angles))
    (if (float-eq angle prev-angle)
	(unique-angles (cdr angles) prev-angle unique)
	(unique-angles (cdr angles) angle (1+ unique)))
    unique))

(defun visible-points (from points)
  (let ((angles (all-angles from (remove from points :test #'equalp))))
    (unique-angles (cdr angles) (car angles) 1)))

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

(defun all-angles-with-points (from points)
  (let ((angles (mapcar (lambda (p) (cons (angle-between-points from p) p)) points)))
    (sort angles (lambda (p1 p2) (> (car p1) (car p2))))))

(defun collect-points (from angles prev-angle &optional points result)
  (flet ((cons-sorted-points () (cons (sort points (sort-by-distance-fn from)) result)))
    (if-let (angle-point (car angles))
	(let ((angle (car angle-point))
	      (point (cdr angle-point)))
	  (if (float-eq angle prev-angle)
	      (collect-points from (cdr angles) prev-angle (cons point points) result)
	      (collect-points from (cdr angles) angle (list point) (cons-sorted-points))))
      (let ((final-result (if points (cons-sorted-points) result)))
	(make-array (list (length final-result)) :initial-contents (nreverse final-result))))))

(defun vaporized (points vec-size nth &optional (index 0) prev-point)
  (if (zerop nth)
      prev-point
      (if-let (point (pop (svref points (mod index vec-size))))
	(vaporized points vec-size (1- nth) (1+ index) point)
	(vaporized points vec-size nth (1+ index)))))

(defun vaporized-point (points nth)
  (let* ((station-point (cadr (best-location-point points)))
	 (angles (all-angles-with-points station-point (remove station-point points :test #'equalp)))
	 (pts (collect-points station-point angles (caar angles))))
    (vaporized pts (length pts) nth)))

(defun solution-2 ()
  (vaporized-point (read-map (read-input)) 200))
