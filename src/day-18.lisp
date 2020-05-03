(defpackage :day-18
  (:use :cl :advent-of-code :iterate :alexandria :queues)
  (:export :solution-1 :solution-2 :shortest-distance))

(in-package :day-18)

(defun read-input ()
  (read-lines (resource-file #p"day-18-input.txt")))

(defun read-map (lines)
  (iter
    (with map = (make-hash-table :test #'equal))
    (with location = (cons -1 -1))
    (for i :in lines)
    (for y :from 0)
    (iter
      (for x :below (length i))
      (for c = (schar i x))
      (when (char= c #\@)
	(setf location (cons x y))
	(setf c #\.))
      (setf (gethash (cons x y) map) c))
    (finally (return (values map location)))))

(defun new-hash-table ()
  (make-hash-table :test #'equal))

(defun is-wall? (map location)
  (char= (gethash location map) #\#))

(defun is-door-locked? (map location keys)
  (let ((door (gethash location map)))
    (and (characterp door) (upper-case-p door) (null (gethash (char-downcase door) keys)))))

(defun found-new-key? (map location keys)
  (let ((key (gethash location map)))
    (and (characterp key) (lower-case-p key) (null (gethash key keys)))))

(defun is-visited? (visited location)
  (gethash location visited))

(defun locations-to-explore (map visited from keys)
  (remove-if (λ (p) (or
		     (is-wall? map p)
		     (is-visited? visited p)
		     (is-door-locked? map p keys)))
	     (adjacent from)))

(defstruct search-location location distance keys)

(defun compare-search-location (p1 p2)
  (< (search-location-distance p1) (search-location-distance p2)))

(defun explore (queue map visited collected-keys found-keys)
  (if-let (sl (qpop queue))
    (let* ((location (search-location-location sl))
	   (distance (search-location-distance sl))
	   (unvisited (locations-to-explore map visited location collected-keys)))
      (when (and (found-new-key? map location collected-keys))
	(setf (gethash location found-keys) distance))
      (iter
	(for p in unvisited)
	(setf (gethash p visited) t)
	(qpush queue (make-search-location :location p :distance (1+ distance))))
      (setf (gethash location visited) t)
      (explore queue map visited collected-keys found-keys))
    found-keys))

(defun explore-area (from map collected-keys)
  (let ((queue (make-queue :priority-queue :compare #'compare-search-location))
	(visited (new-hash-table)))
    (qpush queue (make-search-location :location from :distance 0))
    (setf (gethash from visited) t)
    (explore queue map visited collected-keys (new-hash-table))))

(defun all-keys (map)
  (iter
    (for v :in (hash-table-values map))
    (when (lower-case-p v)
      (collect v))))

(defun collect-key (map distance keys found-key)
  (destructuring-bind (key-loc . key-dist) found-key
    (let* ((new-keys (copy-hash-table keys))
	   (key (gethash key-loc map)))
      (assert (lower-case-p key))
      (setf (gethash key new-keys) t)
      (make-search-location
       :location key-loc
       :distance (+ distance key-dist)
       :keys new-keys))))

(defun search-location-hash-key (search-loc map)
  (let* ((location (search-location-location search-loc))
	 (keys (search-location-keys search-loc)))
    (coerce (cons (gethash location map) (cons #\- (sort (hash-table-keys keys) #'char<))) 'string)))

(defun not-visited? (search-loc map visited)
  (let* ((distance (search-location-distance search-loc))
	 (hash-key (search-location-hash-key search-loc map)))
    (when (or
	   (null (gethash hash-key visited))
	   (> (gethash hash-key visited) distance))
      (setf (gethash hash-key visited) distance))))

(defun search-keys (queue map number-of-keys visited)
  (when-let (sl (qpop queue))
    (let* ((location (search-location-location sl))
	   (distance (search-location-distance sl))
	   (keys (search-location-keys sl)))

      (if (>= (hash-table-count keys) number-of-keys)

	  distance

	  (let* ((found-keys (explore-area location map keys))
		 (new-locations (mapcar (curry #'collect-key map distance keys) (hash-table-alist found-keys))))
	    (iter
	      (for p :in new-locations)
	      (when (not-visited? p map visited)
		(qpush queue p)))

	    (search-keys queue map number-of-keys visited))

	  ))))

(defun shortest-distance (input)
   (multiple-value-bind (map location) (read-map input)
     (let ((sl (make-search-location :location location :distance 0 :keys (make-hash-table)))
	   (queue (make-queue :priority-queue :compare #'compare-search-location))
	   (keys (all-keys map)))
       (qpush queue sl)
       (search-keys queue map (length keys) (make-hash-table :test #'equal)))))

(defun solution-1 ()
  (shortest-distance (read-input)))

(defun solution-2 ())
