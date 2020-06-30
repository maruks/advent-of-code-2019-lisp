(defpackage :day-24
  (:use :cl :aoc :iterate :alexandria)
  (:export :solution-1 :solution-2))

(in-package :day-24)

(defconstant +bug+ #\#)
(defconstant +empty+ #\.)
(defconstant +sub-level+ #\?)

(defconstant +width+ 5)
(defconstant +height+ 5)

(defun read-input ()
  (read-lines (resource-file #p"day-24-input.txt")))

(defun adjacent-bugs (xp yp map)
  (iter
    (for delta :in '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
    (for x = (+ xp (car delta)))
    (for y = (+ yp (cdr delta)))
    (counting (and (>= x 0) (>= y 0) (< x +width+) (< y +height+) (char= +bug+ (aref map y x))))))

(defun iterate-map (map)
  (iter
    (with new-map = (make-array (list +height+ +width+) :element-type 'character))
    (for x :below +width+)
    (iter
      (for y :below +height+)
      (let* ((adjacent (adjacent-bugs x y map))
	     (prev-bug? (char= +bug+ (aref map y x)))
	     (next-bug? (if prev-bug?
			    (eql 1 adjacent)
			    (or (eql adjacent 1) (eql adjacent 2)))))
	(setf (aref new-map y x) (if next-bug? +bug+ +empty+))))
    (finally (return new-map))))

(defun rating (map)
  (iter
    (with result = 0)
    (with index = 0)
    (for y :below +height+)
    (iter
      (for x :below +width+)
      (when (char= +bug+ (aref map y x))
	(setq result (logior result (ash 1 index))))
      (incf index))
    (finally (return result))))

(defun read-map (lines)
  (iter
    (with map = (make-array (list (length (car lines)) (length lines)) :element-type 'character))
    (for i :in lines)
    (for y :from 0)
    (iter
      (for x :below (length i))
      (setf (aref map y x) (schar i x)))
    (finally (return map))))

(defun first-layout (ratings map)
  (let* ((rating (rating map)))
    (if (gethash rating ratings)
	rating
	(progn
	  (setf (gethash rating ratings) t)
	  (first-layout ratings (iterate-map map))))))

(defun solution-1 ()
  (let ((map (read-map (read-input))))
    (first-layout (make-hash-table) map)))

;; part 2

(defun read-map-2 (lines)
  (let ((map (read-map lines)))
    (setf (aref map 2 2) +sub-level+)
    map))

(defun empty-map ()
  (let ((map (make-array (list +height+ +width+) :element-type 'character :initial-element +empty+)))
    (setf (aref map 2 2) +sub-level+)
    map))

(defun embedded-map-bugs (map location)
  (let ((deltas (ecase location
		  (:top (mapcar (curry #'cons 0) (iota +width+)))
		  (:bottom (mapcar (curry #'cons (1- +height+)) (iota +width+)))
		  (:left (mapcar (rcurry #'cons 0) (iota +height+)))
		  (:right (mapcar (rcurry #'cons (1- +width+)) (iota +height+))))))
    (iter
      (for d :in deltas)
      (counting (char= +bug+ (aref map (car d) (cdr d)))))))

(defun adjacent-bugs-2 (xp yp level all-maps)
  (flet ((count-bugs (y x location)
	   (multiple-value-bind (read-map read-x read-y) (if (and (>= x 0) (>= y 0) (< x +width+) (< y +height+))
							     (values (gethash level all-maps) x y)
							     (values
							      (gethash (1- level) all-maps)
							      (cond
								((minusp x) 1)
								((>= x +width+) 3)
								(t 2))
							      (cond
								((minusp y) 1)
								((>= y +height+) 3)
								(t 2))))
	     (let* ((c (aref read-map read-y read-x)))
	       (cond ((char= c +sub-level+) (embedded-map-bugs (gethash (1+ level) all-maps) location))
		     ((char= c +bug+) 1)
		     (t 0))))))
    (iter
      (for delta :in '((:bottom (-1 . 0)) (:top (1 . 0)) (:right (0 . -1)) (:left (0 . 1))))
      (for x = (+ xp (cdadr delta)))
      (for y = (+ yp (caadr delta)))
      (for location = (car delta))
      (sum (count-bugs y x location)))))

(defun iterate-map-2 (level all-maps)
  (iter
    (with map = (gethash level all-maps))
    (with new-map = (empty-map))
    (for x :below +width+)
    (iter
      (for y :below +height+)
      (let* ((adjacent (adjacent-bugs-2 x y level all-maps))
	     (prev-bug? (char= +bug+ (aref map y x)))
	     (next-bug? (if prev-bug?
			    (eql 1 adjacent)
			    (or (eql adjacent 1) (eql adjacent 2)))))
	(setf (aref new-map y x) (cond
				   ((char= (aref map y x) +sub-level+) +sub-level+)
				   (next-bug? +bug+)
				   (t +empty+)))))
    (finally (return new-map))))

(defun iterate-maps (all-maps iteration)
  (let* ((levels (remove-if (λ (i) (> (abs i) (1+ iteration) )) (hash-table-keys all-maps)))
	 (next-maps (mapcar (λ (level) (iterate-map-2 level all-maps)) levels)))
    (iter
      (for level :in levels)
      (for map :in next-maps)
      (setf (gethash level all-maps) map))))

(defun create-maps (map-0 levels)
  (iter
    (with result = (make-hash-table :size (1+ (* 2 levels))))
    (for i :from 1 :to levels)
    (setf (gethash i result) (empty-map))
    (setf (gethash (- i) result) (empty-map))
    (finally (setf (gethash 0 result) map-0)
	     (return result))))

(defun count-all-bugs (all-maps)
  (flet ((count-bugs (map)
	   (iter outer
	     (for x :below +width+)
	     (iter
	       (for y :below +height+)
	       (in outer (counting (char= +bug+ (aref map y x))))))))
    (let* ((levels (hash-table-keys all-maps))
	   (bugs (mapcar (compose #'count-bugs (rcurry #'gethash all-maps)) levels)))
      (reduce #'+ bugs :initial-value 0))))

(defun part-2 (input iterations)
  (let* ((map (read-map-2 input))
	 (all-maps (create-maps map (1+ iterations))))
    (dotimes (i iterations)
      (iterate-maps all-maps i))
    (count-all-bugs all-maps)))

(defun solution-2 ()
  (part-2 (read-input) 200))
