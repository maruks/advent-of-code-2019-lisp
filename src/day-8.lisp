(defpackage :day-8
  (:use :cl :zpng :iterate :aoc)
  (:export :solution-1 :solution-2))

(in-package :day-8)

(defun read-input ()
  (uiop:read-file-string (resource-file "day-8-input.txt")))

(defparameter *width* 25)
(defparameter *height* 6)
(defparameter *layer-size* (* *width* *height*))

(defun number-of-digits (image digit layer)
  (iter
    (with offset = (* *layer-size* layer))
    (for i :from offset :below (+ *layer-size* offset))
    (counting (char= digit (schar image i)))))

(defun fewest-digits-layer (image digit layers)
  (iter
    (for i :below layers)
    (finding i :minimizing (number-of-digits image digit i))))

(defun solution-1 ()
  (let* ((input (read-input))
	 (size (length input))
	 (layers (truncate size *layer-size*))
	 (layer (fewest-digits-layer input #\0 layers)))
    (* (number-of-digits input #\1 layer) (number-of-digits input #\2 layer))))

(defun solution-2 ()
  (let ((input (read-input))
	(file #p"/tmp/day-8.png"))
    (draw-image file input)
    file))

(defun get-pixel (x y image &optional (layer 0))
  (let* ((offset (* layer *layer-size*))
	 (idx (+ offset x (* y *width*)))
	 (p (schar image idx)))
    (ecase (intern (string p) "KEYWORD")
      (:0 255)
      (:1 0)
      (:2 (get-pixel x y image (1+ layer))))))

(defun draw-image (file image)
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
	  (write-pixel (list (get-pixel x y image)) png)))
      (finish-png png))))
