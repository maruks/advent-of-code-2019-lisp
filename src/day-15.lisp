(defpackage :day-15
  (:use :cl :advent-of-code :iterate :alexandria :queues)
  (:import-from :day-5 :run-program-1 :allocate-program-memory)
  (:export :solution-1 :solution-2))

(in-package :day-15)

(defun read-input ()
  (read-code (resource-file #p"day-15-input.txt")))

(defun do-step (program ip direction)
  (multiple-value-bind (next-ip result status) (run-program-1 program (list direction) ip)
    (when (eq status :output)
      (values result next-ip))))

(defstruct droid program ip location distance)

(defun move-droid (droid direction)
  (let ((program (copy-array (droid-program droid)))
	(next-distance (1+ (droid-distance droid)))
	(next-location (-> droid (droid-location) (move-location direction))))
    (multiple-value-bind (out next-ip) (do-step program (droid-ip droid) direction)
      (ecase out
	(0 nil)
	(1 (make-droid :program program
		       :ip next-ip
		       :location next-location
		       :distance next-distance))
	(2 (cons next-distance next-location))))))

(defun move-location (point direction)
  (ecase direction
    (1 (make-point :x (point-x point) :y (1- (point-y point))))
    (2 (make-point :x (point-x point) :y (1+ (point-y point))))
    (3 (make-point :x (1- (point-x point)) :y (point-y point)))
    (4 (make-point :x (1+ (point-x point)) :y (point-y point)))))

(defun search-oxygen (queue visited)
  (when-let (droid (qpop queue))
    (let* ((location (droid-location droid))
	   (moved (->> (iota 4 :start 1)
		    (delete-if (lambda (d) (gethash (move-location location d) visited)))
		    (mapcar (curry #'move-droid droid))
		    (delete-if #'null )))
	   (found-distance (find-if #'consp moved)))
      (or found-distance
	  (progn
	    (iter
	      (for m in moved)
	      (setf (gethash (droid-location m) visited) t)
	      (qpush queue m))
	    (setf (gethash location visited) t)
	    (search-oxygen queue visited))))))

(defun shortest-path (program)
  (let ((queue (make-queue :simple-queue))
	(visited (make-hash-table :test #'equalp)))
    (qpush queue (make-droid :program program :ip 0 :distance 0 :location (make-point :x 0 :y 0)))
    (search-oxygen queue visited)))

(defun solution-1 ()
  (let ((input (allocate-program-memory (read-input))))
    (shortest-path input)))

(defun print-xy (x y map what)
  (let ((rx (- x 30))
	(ry (- y 30)))
    (cond
      ((and (= 0 rx) (= 0 ry)) #\X)
      ((and (= -20 rx) (= 18 ry)) #\O)
      (t (if (eq what (gethash (cons rx ry) map)) #\# #\Space)))))

(defun print-map-2 (map what)
  (iter
    (for y below 50)
    (iter
      (for x below 50)
      (format t "~a" (print-xy x y map what)))
    (format t "~%")))

(defun move-droid-2 (droid direction)
  (let ((program (copy-array (droid-program droid)))
	(next-distance (1+ (droid-distance droid)))
	(next-location (move-location (droid-location droid) direction)))
    (multiple-value-bind (out next-ip) (do-step program (droid-ip droid) direction)
      (if (zerop out)
	  (cons (point-x next-location) (point-y next-location))
	  (make-droid :program program
		      :ip next-ip
		      :location next-location
		      :distance next-distance)))))

(defun explore-map (droid visited map)
  (let* ((location (droid-location droid))
	 (moved (->> (iota 4 :start 1)
		  (delete-if (λ (d) (gethash (move-location location d) visited)))
		  (mapcar (curry #'move-droid-2 droid))))
	 (walls (remove-if-not #'consp moved))
	 (droids (delete-if #'consp moved)))
    (iter
      (for w in walls)
      (setf (gethash w map) :wall))
    (iter
      (for m in droids)
      (setf (gethash (cons (point-x (droid-location m)) (point-y (droid-location m))) map) :empty)
      (setf (gethash (droid-location m) visited) t)
      (explore-map m visited map))))

(defun fill-with-oxygen (from map &optional (time 0))
  (if from
      (let ((filled (delete-if-not
		     (λ (p) (eq :empty (gethash p map)))
		     (delete-duplicates (mappend #'adjacent from) :test #'equal))))
	(iter
	  (for f in from)
	  (setf (gethash f map) :oxygen))
	(fill-with-oxygen filled map (1+ time)))
      (1- time)))

(defun solution-2 ()
  (let ((map (make-hash-table :test #'equal))
	(visited (make-hash-table :test #'equalp))
	(input (allocate-program-memory (read-input))))
    (explore-map
     (make-droid :program input :ip 0 :distance 0 :location (make-point :x 0 :y 0))
     visited
     map)
    ;; (print-map-2 map :wall)
    (fill-with-oxygen (list (cons -20 18)) map)))
