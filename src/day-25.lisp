(defpackage #:day-25
  (:use #:cl #:aoc #:iterate #:alexandria #:queues)
  (:import-from #:day-5 #:run-program-1 #:allocate-program-memory)
  (:import-from #:str #:lines #:starts-with? #:concat #:containsp)
  (:export #:solution-1 #:solution-2))

(in-package #:day-25)

(defun read-input ()
  (read-code (resource-file #p"day-25-input.txt")))

(defstruct droid
  (ip 0 :type integer)
  (code nil :type (vector integer)))

(defun read-items (lines &optional result)
  (if-let (line (car lines))
    (if (starts-with? "- " line)
	(read-items (cdr lines) (cons (subseq line 2) result))
	(nreverse result))
    (nreverse result)))

(defun scan-output (prefix lines)
  (read-items (cdr (member prefix lines :test #'string=))))

(defun room-name (lines)
  (find-if (curry #'starts-with? "== ") lines))

(defun input->ascii (input)
  (when input
    (append
     (mapcar #'char-code (coerce input 'list))
     '(10))))

(defun run-droid (droid &optional input output)
  (multiple-value-bind (next-ip result status) (run-program-1 (droid-code droid) (input->ascii input) (droid-ip droid))

    (when (not (eq status :halt))
	(setf (droid-ip droid) next-ip))

    (ecase status
      (:halt (coerce (mapcar #'code-char (nreverse output)) 'string))
      (:output (run-droid droid nil (cons result output)))
      (:input (coerce (mapcar #'code-char (nreverse output)) 'string)))))

(defparameter *security-checkpoint* "== Security Checkpoint ==")

(defparameter *skip-security-checkpoint* T)

(defparameter *map* nil)

(defun find-nearest-unopened-door (queue visited)
  (when-let (room-doors (qpop queue))
    (destructuring-bind (room &rest doors) room-doors
      (let* ((all-door-rooms (gethash room *map*))
	     (door-rooms (if (and *skip-security-checkpoint* (string= room *security-checkpoint*))
				  (remove-if (compose (curry #'string= "south") #'car) all-door-rooms)
				  all-door-rooms)))
	(if-let (door-room (find-if (compose (curry #'eq :unopened) #'cdr) door-rooms))
	  (nreverse (cons (car door-room) doors))
	  (progn
	    (dolist (rd door-rooms)
	      (destructuring-bind (d . r) rd
		(when (null (gethash r visited))
		  (qpush queue (cons r (cons d doors)))
		  (setf (gethash r visited) T))))
	    (find-nearest-unopened-door queue visited)))))))

(defun nearest-unopened-door (room)
  (let ((queue (make-queue :simple-queue)))
    (qpush queue (list room))
    (find-nearest-unopened-door queue (make-hash-table :test #'equal))))

(defun visit (room doors prev-room prev-door)
  (multiple-value-bind (room-doors found?) (gethash room *map*)
    (declare (ignore room-doors))
    (when (null found?)
      (setf (gethash room *map*) (mapcar (rcurry #'cons :unopened) doors)))
    (when (and prev-room prev-door)
      (let* ((prev-room-doors (gethash prev-room *map*))
	     (door-next-room (assoc prev-door prev-room-doors :test #'string=)))
	(setf (cdr door-next-room) room)))))

(defun take-items (items droid)
  (when-let (item (car items))
    (run-droid droid (concat "take " item))
    (take-items (cdr items) droid)))

(defun collect-all-items (droid &optional commands prev-room)
  (let* ((out (run-droid droid (car commands)))
	 (lines (lines out))
	 (room (room-name lines))
	 (doors (scan-output "Doors here lead:" lines))
	 (items (scan-output "Items here:" lines))
	 (safe-items (remove-if (λ (item) (or (string= item "escape pod")
					      (string= item "infinite loop")
					      (string= item "molten lava")
					      (string= item "photons")
					      (string= item "giant electromagnet")))
				items)))
    (when safe-items
      (take-items safe-items droid))

    (visit room doors prev-room (car commands))

    (let ((next-commands (or (cdr commands) (nearest-unopened-door room))))
      (if next-commands
	  (collect-all-items droid next-commands room)
	  room))))

(defun execute-commands (droid commands)
  (when-let (cmd (car commands))
    (run-droid droid cmd)
    (execute-commands droid (cdr commands))))

(defun drop-all (droid)
  (let* ((out (run-droid droid "inv"))
	 (lines (lines out))
	 (items (scan-output "Items in your inventory:" lines)))
    (execute-commands droid (mapcar (curry #'concat "drop ") items))
    items))

(defun select-items (droid items attempt &optional selected-items)
  (if (zerop attempt)
      (execute-commands droid (mapcar (curry #'concat "take ") selected-items))
      (if (zerop (logand attempt 1))
	  (select-items droid (cdr items) (ash attempt -1) selected-items)
	  (select-items droid (cdr items) (ash attempt -1) (cons (car items) selected-items)))))

(defun get-past-sensor (droid items attempt)
  (drop-all droid)
  (select-items droid items attempt)
  (let* ((out (run-droid droid "south"))
	 (lines (lines out))
	 (ejected? (find-if (curry #'containsp "you are ejected back to the checkpoint") lines)))
    (if ejected?
	(get-past-sensor droid items (1+ attempt))
	(apply #'concat lines))))

(defun solution-1 ()
  (let* ((code (read-input))
	 (program (allocate-program-memory code 6000))
	 (*map* (make-hash-table :test #'equal))
	 (droid (make-droid :code program))
	 (room (collect-all-items droid)))
    (setf *skip-security-checkpoint* nil)
    (execute-commands droid (nearest-unopened-door room))
    (get-past-sensor droid (drop-all droid) 0)))

(defun solution-2 ()
  "Congratulations! You've finished every puzzle in Advent of Code 2019!")
