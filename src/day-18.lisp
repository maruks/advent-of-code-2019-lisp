(defpackage :day-18
  (:use :cl :advent-of-code :iterate :alexandria :queues)
  (:export :solution-1 :solution-2 :shortest-distance))

(in-package :day-18)

(defun read-input ()
  (read-lines (resource-file #p"day-18-input.txt")))

(defstruct search-location location distance)

(defun read-map (lines)
  (iter
    (with map = (make-hash-table :test #'equal))
    (with start-location = (cons -1 -1))
    (with locations)
    (for i :in lines)
    (for y :from 0)
    (iter
      (for x :below (length i))
      (for c = (schar i x))
      (cond
	((char= c #\@) (setf start-location (cons x y)))
	((alpha-char-p c) (push (cons x y) locations)))
      (setf (gethash (cons x y) map) c))
    (finally (return (values map start-location locations)))))

(defun is-wall? (map location)
  (char= (gethash location map) #\#))

(defun locations-to-explore (map visited from distance)
  (let ((locations (remove-if (λ (p) (or
				      (is-wall? map p)
				      (gethash p visited)))
			      (adjacent from))))
    (mapcar (λ (p) (make-search-location
		    :location p
		    :distance (1+ distance)))
	    locations)))

(defun populate-graph (queue from map graph visited)
  (when-let (sl (qpop queue))
    (let* ((location (search-location-location sl))
	   (distance (search-location-distance sl))
	   (node (gethash location map)))
      (if (and (alpha-char-p node) (null (equal from location)))
	  (progn
	    (push (cons node distance) (gethash (gethash from map) graph))
	    (populate-graph queue from map graph visited))
	  (let ((new-locations (locations-to-explore map visited location distance)))
	    (iter
	      (for p :in new-locations)
	      (setf (gethash (search-location-location p) visited) t)
	      (qpush queue p))
	    (populate-graph queue from map graph visited))))))

(defun build-graph (map locations)
  (iter
    (with result = (make-hash-table :test #'equal))
    (for p :in locations)
    (let ((queue (make-queue :simple-queue))
	  (visited (make-hash-table :test #'equal)))
      (qpush queue (make-search-location :location p :distance 0))
      (populate-graph queue p map result visited))
    (finally (return result))))

(defun add-key (keys key-char)
  (let* ((code (- (char-code key-char) (char-code #\a)))
	 (bitmask (ash 1 code)))
    (logior keys bitmask)))

(defun is-key? (key)
  (and (characterp key) (lower-case-p key)))

(defun has-key? (key keys)
  (and (is-key? key)
       (logbitp (- (char-code key) (char-code #\a)) keys)))

(defun all-keys (graph)
  (iter
    (for v :in (hash-table-keys graph))
    (when (lower-case-p v)
      (collect v))))

(defun all-keys-number (ks)
  (reduce #'add-key ks :initial-value 0))

(defun is-locked-door? (door keys)
  (and (characterp door)
       (upper-case-p door)
       (null (has-key? (char-downcase door) keys))))

(defvar *shortest-path-cache*)

(defun shortest-path (from graph keys all-keys-number)
  (memoize-function *search-keys-cache* (logior (ash (char-code from) 26) keys)
    	    (if
	     (eq keys all-keys-number)
	     0
	     (let ((reachable-keys (search-keys graph from keys)))
	       (reduce #'min (mapcar (λ (kd)
				       (destructuring-bind (k . d) kd
					 (+ d
					    (shortest-path k graph (add-key keys k) all-keys-number))))
				     reachable-keys)
		       :initial-value 10000)))))

(defun add-to-results (kd results)
  (destructuring-bind (k . d) kd
    (when (null (gethash k results))
      (setf (gethash k results) d))))

(defun compare-dist (p1 p2)
  (< (cdr p1) (cdr p2)))

(defvar *search-keys-cache*)

(defun search-keys (graph from keys)
  (memoize-function *search-keys-cache* (logior (ash (char-code from) 26) keys)
    (let ((result (make-hash-table :test #'equal))
	  (visited (make-hash-table :test #'equal))
	  (queue (make-queue :priority-queue :compare #'compare-dist)))
      (qpush queue (cons from 0))
      (do-search queue graph keys visited result)
      (remove-if-not (compose #'is-key? #'car) (hash-table-alist result)))))

(defun do-search (queue graph keys visited results)
  (when-let (kd (qpop queue))
    (destructuring-bind (k . d) kd
	(let* ((new-locations (remove-if
			       (λ (kd) (or (gethash (car kd) visited)
					   (is-locked-door? (car kd) keys)))
			       (gethash k graph))))
	  (iter
	    (for nkd :in new-locations)
	    (for nk = (car nkd))
	    (for new-kd = (cons nk (+ d (cdr nkd))))
	    (if (and (is-key? nk) (null (has-key? nk keys)))
		(add-to-results new-kd results)
		(progn (qpush queue new-kd)
		       (setf (gethash nk visited) t))))
	  (do-search queue graph keys visited results)))))

(defun shortest-distance (input)
  (let ((*shortest-path-cache* (make-hash-table))
	(*search-keys-cache* (make-hash-table)))
    (multiple-value-bind (map start-location locations) (read-map input)
      (let* ((graph (build-graph map (cons start-location locations)))
	     (all-keys-number (all-keys-number (all-keys graph))))
	(shortest-path #\@ graph 0 all-keys-number)))))

(defun solution-1 ()
  (shortest-distance (read-input)))

(defun solution-2 ())
