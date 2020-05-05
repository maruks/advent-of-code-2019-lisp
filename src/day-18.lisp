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

(defun all-keys (map)
  (iter
    (for v :in (hash-table-values map))
    (when (lower-case-p v)
      (collect v))))

(defun is-wall? (map location)
  (char= (gethash location map) #\#))

(defun is-locked-door? (map location keys)
  (let ((door (gethash location map)))
    (and (characterp door) (upper-case-p door) (null (has-key? (char-downcase door) keys)))))

(defun has-key? (key-char keys)
  (let ((code (- (char-code key-char) (char-code #\a))))
    (logbitp code keys)))

(defun add-key (keys key-char)
  (let* ((code (- (char-code key-char) (char-code #\a)))
	 (bitmask (ash 1 code)))
    (logior keys bitmask)))

(defun all-keys-number (ks)
  (reduce #'add-key ks :initial-value 0))

(defun is-key? (map location)
  (let ((key (gethash location map)))
    (when (and (characterp key) (lower-case-p key))
      key)))

(defun location-hash-key (location keys)
  (logior (ash (car location) 33) (ash (cdr location) 26) keys))

(defun is-visited? (visited location keys)
  (gethash (location-hash-key location keys) visited))

(defun add-to-visited (sl visited)
  (let* ((location (search-location-location sl))
	 (keys (search-location-keys sl))
	 (hash-key (location-hash-key location keys)))
    (setf (gethash hash-key visited) t)))

(defstruct search-location location distance keys)

(defun locations-to-explore (map visited from keys distance)
  (let ((locations (remove-if (λ (p) (or
				      (is-wall? map p)
				      (is-visited? visited p keys)
				      (and (when-let (k (is-key? map p))
					     (is-visited? visited p (add-key keys k))))
				      (is-locked-door? map p keys)))
			      (adjacent from))))
    (mapcar (λ (p) (make-search-location
		    :location p
		    :distance (1+ distance)
		    :keys (if-let (k (is-key? map p))
			    (add-key keys k)
			    keys)))
	    locations)))

(defun search-keys (queue map all-keys-number visited)
  (when-let (sl (qpop queue))
    (let* ((location (search-location-location sl))
	   (distance (search-location-distance sl))
	   (keys (search-location-keys sl)))
      (if (eq keys all-keys-number)
	  (values distance (hash-table-count visited))
	  (let ((new-locations (locations-to-explore map visited location keys distance)))
	    (iter
	      (for p :in new-locations)
	      (add-to-visited p visited)
	      (qpush queue p))
	    (search-keys queue map all-keys-number visited))))))

(defun shortest-distance (input)
   (multiple-value-bind (map location) (read-map input)
     (let ((sl (make-search-location :location location :distance 0 :keys 0))
	   (queue (make-queue :simple-queue))
	   (visited (make-hash-table))
	   (all-keys-num (all-keys-number (all-keys map))))
       (qpush queue sl)
       (add-to-visited sl visited)
       (search-keys queue map all-keys-num visited))))

(defun solution-1 ()
  (shortest-distance (read-input)))

(defun solution-2 ())
