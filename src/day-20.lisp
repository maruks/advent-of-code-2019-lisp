(defpackage :day-20
  (:use :cl :advent-of-code :iterate :alexandria :queues)
  (:export :solution-1 :shortest-path-length :shortest-path-length-2 :read-input :solution-2))

(in-package :day-20)

(defconstant +start+ :AA)
(defconstant +end+ :ZZ)
(defconstant +warp-distance+ 1)

(defun read-map (lines)
  (iter
    (with result = (make-hash-table :test #'equal))
    (for s :in lines)
    (for y :from 0)
    (iter
      (for x :below (length s))
      (setf (gethash (cons x y) result) (schar s x)))
    (finally (return result))))

(defun open-passage? (c)
  (and (characterp c) (char= c #\.)))

(defun portal-id (char-1 location-1 char-location-2)
  (destructuring-bind (location-2 . char-2) char-location-2
    (destructuring-bind (x1 . y1) location-1
      (destructuring-bind (x2 . y2) location-2
	(let* ((reversed? (or
			   (and (= x1 x2) (< y2 y1))
			   (and (= y1 y2) (< x2 x1))))
	       (char-list (if reversed? (list char-2 char-1) (list char-1 char-2))))
	  (make-keyword (make-array 2 :initial-contents char-list :element-type 'character)))))))

(defun is-portal? (k v map)
  (flet ((upcase-char? (c)
	   (and (characterp c) (upper-case-p c))))
    (when (upcase-char? v)
      (let* ((adjacent (mapcar (λ (v) (cons v (gethash v map))) (adjacent k)))
	     (chars (remove-if-not (compose #'upcase-char? #'cdr) adjacent))
	     (passages (remove-if-not (compose #'open-passage? #'cdr) adjacent)))
	(when (and (= 1 (length chars) (length passages)))
	  (let ((portal (portal-id v k (car chars))))
	    (cons portal (caar passages))))))))

(defun find-portals (map)
  (iter
   (for (k v) :in-hashtable map)
   (when-let (portal (is-portal? k v map))
     (collect portal))))

(defun read-input (file)
  (read-lines (resource-file file)))

(defun add-warp-distances (portals graph)
  (when-let (portal-1 (car portals))
    (when-let (portal-2 (cadr portals))
      (destructuring-bind (name-1 . location-1) portal-1
	(destructuring-bind (name-2 . location-2) portal-2
	  (when (eq name-1 name-2)
            (push (cons location-1 +warp-distance+) (gethash location-2 graph))
            (push (cons location-2 +warp-distance+) (gethash location-1 graph)))
	  (add-warp-distances (cdr portals) graph))))))

(defstruct search-location location distance)

(defun locations-to-explore (map visited from distance)
  (let ((locations (remove-if (λ (p) (or
				      (null (open-passage? (gethash p map)))
				      (gethash p visited)))
			      (adjacent from))))
    (mapcar (λ (p) (make-search-location
		    :location p
		    :distance (1+ distance)))
	    locations)))

(defun shortest-paths (queue map graph visited results)
  (if-let (sl (qpop queue))
    (let* ((location (search-location-location sl))
	   (distance (search-location-distance sl))
	   (locations (locations-to-explore map visited location distance)))
      (when (and (gethash location graph)
		 (< distance (gethash location results 1000000))
		 (plusp distance))
	(setf (gethash location results) distance))
      (setf (gethash location visited) t)
      (dolist (p locations)
	(qpush queue p))
      (shortest-paths queue map graph visited results))
    results))

(defun new-queue (elem &rest queue-args)
  (let ((queue (apply #'make-queue queue-args)))
    (qpush queue elem)
    queue))

(defun add-paths (from-locations map graph)
  (when-let (from (car from-locations))
    (let ((paths (shortest-paths
		  (new-queue (make-search-location :location from :distance 0) :simple-queue)
		  map
		  graph
		  (make-hash-table :test #'equal)
		  (make-hash-table :test #'equal))))
      (iter
	(for (k v) :in-hashtable paths)
	(push (cons k v) (gethash from graph)))
      (add-paths (cdr from-locations) map graph))))

(defun build-graph (map portals)
  (let ((graph (make-hash-table :test #'equal))
	(start-location (cdar portals))
	(end-location (cdar (last portals))))
    (add-warp-distances portals graph)
    (setf (gethash start-location graph) nil)
    (setf (gethash end-location graph) nil)
    (add-paths (hash-table-keys graph) map graph)
    (values graph start-location end-location)))

(defun find-shortest-path-length (queue graph end visited)
  (when-let (sl (qpop queue))
    (let ((location (search-location-location sl))
	  (distance (search-location-distance sl)))
      (if (equal location end)
	  distance
	  (let ((locations (gethash location graph)))
	    (iter
	      (for (p . d) :in locations)
	      (when (null (gethash p visited))
		(qpush queue (make-search-location :location p :distance (+ distance d)))))
	    (setf (gethash location visited) t)
	    (find-shortest-path-length queue graph end visited))))))

(defun compare-search-locations (sl-1 sl-2)
  (< (search-location-distance sl-1) (search-location-distance sl-2)))

(defun shortest-path-length (input)
  (let* ((map (read-map input))
	 (portals (sort (find-portals map) (λ (p1 p2) (string< (string (car p1)) (string (car p2)))))))
    (multiple-value-bind (graph start end) (build-graph map portals)
      (find-shortest-path-length
       (new-queue (make-search-location :location start :distance 0) :priority-queue :compare #'compare-search-locations)
       graph
       end
       (make-hash-table :test #'equal)))))

(defun solution-1 ()
  (shortest-path-length (read-input #p"day-20-input.txt")))

;; --------------------------------- PART 2 ---------------------------------

(defun locations-to-explore-2 (map visited from)
  (remove-if (λ (p) (or
		     (char= #\# (gethash p map))
		     (gethash p visited)))
	     (adjacent from)))

(defun center-location (input)
  (let ((width (iter
		 (for i :in input)
		 (maximize (length i)))))
    (cons (truncate width 2) (truncate (length input) 2))))

(defun inner-portals (queue map visited result)
  (if-let (location (qpop queue))
    (let ((locations (locations-to-explore-2 map visited location))
	  (portal? (char= #\. (gethash location map))))
      (if portal?
	  (setf (gethash location result) t)
	  (dolist (p locations)
	    (when (null (gethash p visited))
	      (qpush queue p)
	      (setf (gethash p visited) t))))
      (setf (gethash location visited) t)
      (inner-portals queue map visited result))
    result))

(defun find-inner-portals (map center)
  (inner-portals (new-queue center :simple-queue) map (make-hash-table :test #'equal) (make-hash-table :test #'equal)))

(defun locations-to-explore-3 (locations from inner-portals level)
  (let ((result (mapcar (λ (loc-dist)
		    (destructuring-bind (location . distance) loc-dist
		      (let ((next-level (if (= +warp-distance+ distance)
					    (if (gethash from inner-portals) (1+ level) (1- level))
					    level)))
			(cons (cons location next-level) distance))))
		  locations)))
    (remove-if #'minusp result :key #'cdar)))

(defun find-shortest-path-length-2 (queue graph end inner-portals visited)
  (when-let (sl (qpop queue))
    (let ((location-level (search-location-location sl))
	  (distance (search-location-distance sl)))
      (destructuring-bind (location . level) location-level
	(if (and (equal location end) (zerop level))
	    distance
	    (let ((locations (locations-to-explore-3 (gethash location graph) location inner-portals level)))
	      (iter
		(for (p . d) :in locations)
		(when (null (gethash p visited))
		  (qpush queue (make-search-location :location p :distance (+ distance d)))))
	      (setf (gethash location-level visited) t)
	      (find-shortest-path-length-2 queue graph end inner-portals visited)))))))

(defun shortest-path-length-2 (input)
  (let* ((map (read-map input))
	 (center (center-location input))
	 (inner-portals (find-inner-portals map center))
	 (portals (sort (find-portals map) (λ (p1 p2) (string< (string (car p1)) (string (car p2)))))))
    (multiple-value-bind (graph start end) (build-graph map portals)
      (find-shortest-path-length-2
       (new-queue (make-search-location :location (cons start 0) :distance 0) :priority-queue :compare #'compare-search-locations)
       graph
       end
       inner-portals
       (make-hash-table :test #'equal)))))

(defun solution-2 ()
  (shortest-path-length-2 (read-input #p"day-20-input.txt")))
