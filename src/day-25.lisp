(defpackage #:day-25
  (:use #:cl #:aoc #:iterate #:alexandria #:queues)
  (:import-from #:intcode #:file->program #:run-program #:program-status)
  (:import-from #:str #:lines #:starts-with? #:concat #:containsp)
  (:import-from #:ppcre #:all-matches-as-strings #:scan-to-strings)
  (:export #:solution-1 #:solution-2))

(in-package #:day-25)

(defun read-input ()
  (file->program #p"day-25-input.txt" 250))

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

(defun run-droid (program &optional input)
  (let ((result (run-program program :input (input->ascii input))))
    (coerce (mapcar #'code-char result) 'string)))

(define-constant +security-checkpoint+ "== Security Checkpoint ==" :test #'string=)

(defparameter *skip-security-checkpoint* T)

(defparameter *map* nil)

(defun find-nearest-unopened-door (queue visited)
  (when-let (room-doors (qpop queue))
    (destructuring-bind (room &rest doors) room-doors
      (let* ((all-door-rooms (gethash room *map*))
	     (door-rooms (if (and *skip-security-checkpoint* (string= room +security-checkpoint+))
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

(defun take-items (items program)
  (when-let (item (car items))
    (run-droid program (concat "take " item))
    (take-items (cdr items) program)))

(defun collect-all-items (program &optional commands prev-room)
  (let* ((out (run-droid program (car commands)))
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
      (take-items safe-items program))

    (visit room doors prev-room (car commands))

    (let ((next-commands (or (cdr commands) (nearest-unopened-door room))))
      (if next-commands
	  (collect-all-items program next-commands room)
	  room))))

(defun execute-commands (program commands)
  (when-let (cmd (car commands))
    (run-droid program cmd)
    (execute-commands program (cdr commands))))

(defun drop-all (program)
  (let* ((out (run-droid program "inv"))
	 (lines (lines out))
	 (items (scan-output "Items in your inventory:" lines)))
    (execute-commands program (mapcar (curry #'concat "drop ") items))
    items))

(defun select-items (program items attempt &optional selected-items)
  (if (zerop attempt)
      (execute-commands program (mapcar (curry #'concat "take ") selected-items))
      (if (zerop (logand attempt 1))
	  (select-items program (cdr items) (ash attempt -1) selected-items)
	  (select-items program (cdr items) (ash attempt -1) (cons (car items) selected-items)))))

(defun get-past-sensor (program items &optional (attempt 0))
  (drop-all program)
  (select-items program items attempt)
  (let* ((out (run-droid program "south"))
	 (lines (lines out))
	 (ejected? (find-if (curry #'containsp "you are ejected back to the checkpoint") lines)))
    (if ejected?
	(get-past-sensor program items (1+ attempt))
	(apply #'concat lines))))

(defparameter *regex* #r"You should be able to get in by typing (\\d+)")

(defun find-keypad-code (text)
  (multiple-value-bind (match substrings) (scan-to-strings *regex* text)
    (when match
      (aref substrings 0))))

(defun solution-1 ()
  (let* ((program (read-input))
	 (*map* (make-hash-table :test #'equal))
	 (*skip-security-checkpoint* T)
	 (room (collect-all-items program)))
    (setf *skip-security-checkpoint* nil)
    (execute-commands program (nearest-unopened-door room))
    (->> program
      drop-all
      (get-past-sensor program)
      find-keypad-code)))

(defun solution-2 ()
  "Congratulations! You've finished every puzzle in Advent of Code 2019!")
