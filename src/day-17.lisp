(defpackage :day-17
  (:use :cl :aoc :iterate :alexandria)
  (:import-from :day-5 :run-program-collect-results :allocate-program-memory)
  (:export :solution-1 :solution-2))

(in-package :day-17)

(defun read-input ()
  (read-code (resource-file #p"day-17-input.txt")))

(defun populate-map (xs map &optional (x 0) (y 0))
  (if-let (code (car xs))
    (let ((char (code-char code)))
      (if (char= #\Newline char)
	  (populate-map (cdr xs) map 0 (1+ y))
	  (progn
	    (setf (gethash (cons x y) map) char)
	    (populate-map (cdr xs) map (1+ x) y))))
    map))

(defun locate-intersections (map)
  (iter
    (for (k v) :in-hashtable map)
    (when (and
	   (char= v #\#)
	   (every (curry #'char= #\#) (mapcar (rcurry #'gethash map #\G) (adjacent k))))
      (collect k))))

(defun solution-1 ()
  (let* ((input (allocate-program-memory (read-input) 3800))
	 (out (run-program-collect-results input '()))
	 (map (populate-map out (make-hash-table :test #'equal))))
    (reduce #'+ (mapcar (lambda (p) (* (car p) (cdr p))) (locate-intersections map)) :initial-value 0)))

(defun find-robot-location (map)
  (iter
    (for (k v) :in-hashtable map)
    (finding k :such-that (char= v #\^))))

(defun turn-robot (turn dir)
  (cond
    ((and (eq :left turn) (eq dir :up)) :left)
    ((and (eq :left turn) (eq dir :left)) :down)
    ((and (eq :left turn) (eq dir :down)) :right)
    ((and (eq :left turn) (eq dir :right)) :up)
    ((and (eq :right turn) (eq dir :up)) :right)
    ((and (eq :right turn) (eq dir :left)) :up)
    ((and (eq :right turn) (eq dir :down)) :left)
    ((and (eq :right turn) (eq dir :right)) :down)))

(defun forward-location (loc dir)
  (destructuring-bind (x . y) loc
    (ecase dir
      (:left (cons (1- x) y))
      (:right (cons (1+ x) y))
      (:up (cons x (1- y)))
      (:down (cons x (1+ y))))))

(defun next-location (loc dir left-or-right)
  (let ((next-dir (turn-robot left-or-right dir)))
    (forward-location loc next-dir)))

(defun collect-path (map loc dir &optional result)
  (flet ((on-scaffoldp (pos) (char= #\# (gethash pos map #\N))))
    (let ((forward (forward-location loc dir))
	  (left (next-location loc dir :left))
	  (right (next-location loc dir :right)))
      (cond ((on-scaffoldp forward) (collect-path map forward dir (progn (incf (car result)) result)))
	    ((on-scaffoldp left) (collect-path map left (turn-robot :left dir) (cons 1 (cons #\L result))))
	    ((on-scaffoldp right) (collect-path map right (turn-robot :right dir) (cons 1 (cons #\R result))))
	    (t (nreverse result))))))

(defun path-to-string (path)
  (flet ((to-str (x)
	   (etypecase x
	     (integer (write-to-string x))
	     (standard-char (string x)))))
    (apply #'concatenate 'string (mapcar #'to-str path))))

(defun compress-path (str num &optional result)
  (if (plusp num)
      (let* ((idxs (iota 14 :start 2))
	     (results (mapcar (λ (i)
				(let ((s (str:substring 0 i str)))
				  (when (< 2 (str:count-substring s str))
				    (compress-path (str:replace-all s "" str) (1- num) (cons s result)))))
			      idxs)))
	(find-if-not #'null results))
      (when (string= "" str)
	(nreverse result))))

(defun main-routine (cmds str)
  (if-let (cmd (car cmds))
    (main-routine (cdr cmds) (str:replace-all (cdr cmd) (car cmd) str))
    str))

(defun string->ascii (str &optional (idx 0) (prev-comma? t) prev-char-digit?)
  (if (>= idx (length str))
      (list (char-code #\Newline))
      (let* ((chr (schar str idx))
	     (is-digit? (not (null (digit-char-p chr))))
	     (comma? (and (not prev-comma?) (not (and is-digit? prev-char-digit?)))))
	(cons
	 (if comma? (char-code #\,) (char-code chr))
	 (string->ascii str (if comma? idx (1+ idx)) comma? is-digit?)))))

(defun video-feed ()
  (list (char-code #\n) (char-code #\Newline)))

(defun solution-2 ()
  (let* ((input (allocate-program-memory (read-input) 3800))
	 (out (run-program-collect-results (copy-array input) '()))
	 (map (populate-map out (make-hash-table :test #'equal)))
	 (path-str (path-to-string (collect-path map (find-robot-location map) :up)))
	 (compressed-path (compress-path path-str 3))
	 (main-cmd (main-routine (mapcar #'cons '("A" "B" "C") compressed-path) path-str))
	 (program-input (append
			 (string->ascii main-cmd)
			 (mappend #'string->ascii compressed-path)
			 (video-feed))))
    (setf (svref input 0) 2)
    (car (last (run-program-collect-results input program-input)))))
