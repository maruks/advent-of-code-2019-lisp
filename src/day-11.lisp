(defpackage :day-11
  (:use :cl :aoc :iterate :alexandria :zpng)
    (:import-from :day-5 :run-program-1 :allocate-program-memory)
    (:export :solution-1 :solution-2))

(in-package :day-11)

(defun read-input ()
  (read-code (resource-file #p"day-11-input.txt")))

(defun do-step (program ip input)
  (multiple-value-bind (next-ip result-1 status) (run-program-1 program (list input) ip)
    (when (eq status :output)
      (multiple-value-bind (next-ip-2 result-2) (run-program-1 program '() next-ip)
	(list result-1 result-2 next-ip-2)))))

(defun move-robot (x y turn dir)
  (let ((next-dir (cond
		    ((and (eql 0 turn) (eq dir :up)) :left)
		    ((and (eql 0 turn) (eq dir :left)) :down)
		    ((and (eql 0 turn) (eq dir :down)) :right)
		    ((and (eql 0 turn) (eq dir :right)) :up)
		    ((and (eql 1 turn) (eq dir :up)) :right)
		    ((and (eql 1 turn) (eq dir :left)) :up)
		    ((and (eql 1 turn) (eq dir :down)) :left)
		    ((and (eql 1 turn) (eq dir :right)) :down))))
    (values-list (cons next-dir (ecase next-dir
				  (:left (list (1- x) y))
				  (:right (list (1+ x) y))
				  (:up (list x (1- y)))
				  (:down (list x (1+ y))))))))

(defun run-robot (program panels &optional (ip 0) (x 0) (y 0) (direction :up))
  (when-let (results (do-step program ip (gethash (cons x y) panels 0)))
    (destructuring-bind (color turn next-ip) results
	(multiple-value-bind (next-direction next-x next-y) (move-robot x y turn direction)
	  (setf (gethash (cons x y) panels) color)
	  (run-robot program panels next-ip next-x next-y next-direction)))))

(defun solution-1 ()
  (let ((input (allocate-program-memory (read-input) 1200))
	(panels (make-hash-table :test #'equal)))
    (run-robot input panels)
    (hash-table-count panels)))

(defun solution-2 ()
  (let ((input (allocate-program-memory (read-input) 1200))
	(panels (make-hash-table :test #'equal))
	(file #p"/tmp/day-11.png"))
    (setf (gethash (cons 0 0) panels) 1)
    (run-robot input panels)
    (draw-image file panels)
    file))

(defparameter *width* 100)
(defparameter *height* 100)

(defun get-pixel (x y panels)
  (let ((px (- x (truncate *width* 2)))
	(py (- y (truncate *height* 2))))
    (* 255 (gethash (cons px py) panels 0))))

(defun draw-image (file panels)
  (let ((png (make-instance 'pixel-streamed-png
			    :color-type :grayscale
			    :width *width*
			    :height *height*)))
    (with-open-file (stream file
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create
			    :element-type '(unsigned-byte 8))
      (start-png png stream)
      (iter
	(for y :below *height*)
	(iter
	  (for x :below *width*)
	  (write-pixel (list (get-pixel x y panels)) png)))
      (finish-png png))))
