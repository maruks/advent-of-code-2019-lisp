(defpackage :day-14
  (:use :cl :advent-of-code :iterate :alexandria :ppcre)
  (:export :solution-1 :find-required-ore-for-input :solution-2))

(in-package :day-14)

(defparameter *number* #r"\\d+")
(defparameter *chemical* #r"[A-Z]+")

(defstruct (reaction) inputs output amount)

(defparameter *fuel* 'FUEL)
(defparameter *ore* 'ORE)

(defun str->reaction (s)
  (let* ((numbers (mapcar #'parse-integer (reverse (all-matches-as-strings *number* s))))
	 (chemicals (mapcar (rcurry #'intern :day-14) (reverse (all-matches-as-strings *chemical* s))))
	 (inputs (pairlis (cdr chemicals) (cdr numbers))))
    (make-reaction :inputs inputs :output (car chemicals) :amount (car numbers))))

(defun reactions-map (reactions)
  (let ((map (make-hash-table :test #'eq)))
    (iter
      (for r in reactions)
      (setf (gethash (reaction-output r) map) r)
      (finally (return map)))))

(defun required-chemicals (required-amount reaction)
  (let* ((reaction-amount (reaction-amount reaction))
	 (multiplier (ceiling required-amount reaction-amount))
	 (inputs (reaction-inputs reaction))
	 (produced (* multiplier reaction-amount)))
    (values
     (pairlis (mapcar #'car inputs) (mapcar (compose (curry #'* multiplier) #'cdr) inputs))
     (- produced required-amount))))

(defun read-input ()
  (read-lines #'identity (resource-file #p"day-14-input.txt")))

(defun add-chemical (chemical amount chemicals)
  (setf (gethash chemical chemicals) (+ amount (gethash chemical chemicals 0))))

(defvar *reactions*)

(defun remove-leftovers (chemicals leftovers)
  (iter
    (for (leftover-chem leftover-amount) in-hashtable leftovers)
    (when-let (required-amount (gethash leftover-chem chemicals))
      (cond ((< leftover-amount required-amount) (progn
			   (setf (gethash leftover-chem chemicals) (- required-amount leftover-amount))
			   (remhash leftover-chem leftovers)))
 	    ((> leftover-amount required-amount) (progn
			   (setf (gethash leftover-chem leftovers) (- leftover-amount required-amount))
			   (remhash leftover-chem chemicals)))
	    (t (progn
		 (remhash leftover-chem leftovers)
		 (remhash leftover-chem chemicals)))))))

(defun how-much-ore (chemicals leftovers &optional (result 0))
  (if (zerop (hash-table-count chemicals))
      result
      (let* ((chemical (car (hash-table-keys chemicals)))
	     (amount (gethash chemical chemicals)))
	(multiple-value-bind (requirements left-over) (required-chemicals amount (gethash chemical *reactions*))
	  (when (plusp left-over)
	    (add-chemical chemical left-over leftovers))
	  (remhash chemical chemicals)
	  (if (eq (caar requirements) *ore*)
	      (how-much-ore chemicals leftovers (+ result (cdar requirements)))
	      (progn
		(iter
		  (for (req-chem . req-am) in requirements)
		  (add-chemical req-chem req-am chemicals)
		  (remove-leftovers chemicals leftovers))
		(how-much-ore chemicals leftovers result)))))))

(defun find-required-ore (&optional fuel)
  (let ((chemicals (alist-hash-table (reaction-inputs (gethash *fuel* *reactions*)) :test #'eq)))
    (when fuel
      (iter
	(for (c a) in-hashtable chemicals)
	(setf (gethash c chemicals) (* fuel (gethash c chemicals)))))
    (how-much-ore chemicals (make-hash-table :test #'eq))))

(defun parse-input (strs)
  (reactions-map (mapcar #'str->reaction strs)))

(defun find-required-ore-for-input (input)
  (let ((*reactions* (parse-input input)))
    (find-required-ore)))

(defun solution-1 ()
  (find-required-ore-for-input (read-input)))

(defun find-max-fuel (low high target)
  (if (= (1+ low) high)
      low
      (let* ((mid (+ low (truncate (- high low) 2)))
	     (ore (find-required-ore mid)))
	(if (< ore target)
	    (find-max-fuel mid high target)
	    (find-max-fuel low mid target)))))

(defun solution-2 ()
  (let ((*reactions* (parse-input (read-input))))
    (find-max-fuel 2000 20000000 1000000000000)))
