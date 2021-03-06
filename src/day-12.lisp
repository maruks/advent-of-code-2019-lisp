(defpackage #:day-12
  (:use #:cl #:aoc)
  (:import-from #:alexandria #:iota #:curry #:define-constant #:hash-table-values)
  (:import-from #:ppcre #:all-matches-as-strings)
  (:export #:solution-1 #:solution-2))

(in-package #:day-12)

(define-constant +number-of-moons+ 4)

(defun read-input (file)
  (flet ((parse-line (s) (mapcar #'parse-integer (all-matches-as-strings "-?\\d+" s))))
    (read-lines (resource-file file) #'parse-line)))

(defstruct moon position velocity)

(defun moons-vector (lists)
  (flet ((->moon (positions)
	   (make-moon
	    :position (apply #'vector positions)
	    :velocity (make-array '(3) :initial-element 0))))
    (apply #'vector (mapcar #'->moon lists))))

(defun pairs-indices ()
  (flet ((pair-sort (a b) (if (< a b) (cons a b) (cons b a))))
    (let ((map (make-hash-table :test #'equal)))
      (dotimes (x +number-of-moons+)
	(dotimes (y +number-of-moons+)
	  (let ((pair (pair-sort x y)))
	    (when (and (null (gethash pair map)) (/= x y))
	      (setf (gethash pair map) pair)))))
      (hash-table-values map))))

(defun move-moons (moons axis-list)
  (flet ((->position (moon axis) (svref (moon-position (svref moons moon)) axis))
	 (->velocity (moon axis) (svref (moon-velocity (svref moons moon)) axis))
	 (change-velocity (moon axis delta) (incf (svref (moon-velocity (svref moons moon)) axis) delta))
	 (change-position (moon axis delta) (incf (svref (moon-position (svref moons moon)) axis) delta)))
    (dolist (idx (pairs-indices))
      (dolist (axis axis-list)
	(let* ((m-1 (car idx))
	       (m-2 (cdr idx))
	       (p-1 (->position m-1 axis))
	       (p-2 (->position m-2 axis)))
	  (when (/= p-1 p-2)
	    (change-velocity m-1 axis (if (< p-1 p-2) 1 -1))
	    (change-velocity m-2 axis (if (< p-2 p-1) 1 -1))))))
    (dotimes (m +number-of-moons+)
      (dolist (axis axis-list)
	(change-position m axis (->velocity m axis))))
    moons))

(defun run-simulation (moons steps)
  (dotimes (i steps moons)
    (move-moons moons (iota 3))))

(defun energy (moons)
  (flet ((do-sum (moon-slot moon-idx)
	   (reduce #'+ (map 'list #'abs (funcall moon-slot (svref moons moon-idx))))))
    (reduce
     (λ (s i) (+ s (* (do-sum #'moon-velocity i)
		      (do-sum #'moon-position i))))
     (iota +number-of-moons+)
     :initial-value 0)))

(defun solution-1 ()
  (-> #p"day-12-input.txt"
    read-input
    moons-vector
    (run-simulation 1000)
    energy))

(defun positions (moons axis)
  (mapcar (λ (m) (svref (moon-position (svref moons m)) axis)) (iota +number-of-moons+)))

(defun number-of-steps (moons initial-positions cycles &optional (steps 0) (axis-list '(0 1 2)))
  (flet ((->velocity (axis moon)
	   (svref (moon-velocity (svref moons moon)) axis)))
    (if axis-list
	(progn
	  (move-moons moons axis-list)
	  (let ((found (remove-if-not
			(λ (i) (and (every #'zerop (mapcar (curry #'->velocity i) (iota +number-of-moons+)))
				    (equal (positions moons i) (svref initial-positions i))
				    (zerop (svref cycles i))))
			axis-list)))
	    (dolist (i found)
	      (setf (svref cycles i) (1+ steps)))
	    (number-of-steps moons initial-positions cycles (1+ steps) (set-difference axis-list found))))
	(apply #'lcm (coerce cycles 'list)))))

(defun solution-2 ()
  (let ((moons (moons-vector (read-input #p"day-12-input.txt")))
	(cycles (make-array '(3) :initial-element 0))
	(initial-positions (make-array (list +number-of-moons+))))
    (dotimes (i 3)
      (setf (svref initial-positions i) (positions moons i)))
    (number-of-steps moons initial-positions cycles)))
