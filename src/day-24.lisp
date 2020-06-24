(defpackage :day-24
  (:use :cl :aoc :iterate :alexandria :queues)
  (:export :solution-1 :solution-2))

(in-package :day-24)

(defconstant +bug+ #\#)
(defconstant +empty+ #\.)

(defun read-input ()
  (read-lines (resource-file #p"day-24-input.txt")))

(defun adjacent-bugs (xp yp map)
  (destructuring-bind (width height) (array-dimensions map)
    (iter
      (for delta :in '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
      (for x = (+ xp (car delta)))
      (for y = (+ yp (cdr delta)))
      (counting (and (>= x 0) (>= y 0) (< x width) (< y height) (char= +bug+ (aref map y x)))))))

(defun iterate-map (map)
  (destructuring-bind (width height) (array-dimensions map)
    (iter
      (with new-map = (make-array (list width height) :element-type 'character))
      (for x :below width)
      (iter
	(for y :below height)
	(let* ((adjacent (adjacent-bugs x y map))
	       (prev-bug? (char= +bug+ (aref map y x)))
	       (next-bug? (if prev-bug?
			      (eql 1 adjacent)
			      (or (eql adjacent 1) (eql adjacent 2)))))
	  (setf (aref new-map y x) (if next-bug? +bug+ +empty+))))
      (finally (return new-map)))))

(defun rating (map)
  (destructuring-bind (width height) (array-dimensions map)
    (iter
      (with result = 0)
      (with index = 0)
      (for y :below height)
      (iter
	(for x :below width)
	(when (char= +bug+ (aref map y x))
	  (setq result (logior result (ash 1 index))))
	(incf index))
      (finally (return result)))))

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

(defun solution-2 ()
  )
