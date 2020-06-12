(defpackage :day-22
  (:use :cl :aoc :iterate :alexandria)
  (:import-from :str :split :upcase :join)
  (:export :solution-1 :solution-2))

(in-package :day-22)

(defconstant +number-of-cards+ 10007)

(defun read-input (file)
  (flet ((parse-line (line)
	   (let* ((parts (split #\Space line))
		  (fname (->> parts
			   butlast
			   (join #\-)
			   upcase
			   make-keyword))
		  (args (car (last parts))))
	     (cons fname args))))
    (read-lines file #'parse-line)))

(defun deal-into-new (cards)
  (reverse cards))

(defun cut (cards increment)
  (let ((size (if (plusp increment) increment (+ (length cards) increment))))
    (append (subseq cards size) (subseq cards 0 size))))

(defun deal-into-vector (cards increment index result)
  (iter
    (with size = (length result))
    (for c :in cards)
    (for i :initially index :then (mod (+ i increment) size))
    (setf (svref result i) c)))

(defun deal-with-increment (cards increment)
  (let ((vec (make-array (length cards))))
    (deal-into-vector cards increment 0 vec)
    (coerce vec 'list)))

(defun apply-instructions (instructions cards)
  (if-let (instr (car instructions))
    (let ((next-cards (destructuring-bind (fname . arg) instr
			(ecase fname
			  (:deal-into-new (deal-into-new cards))
			  (:deal-with-increment (deal-with-increment cards (parse-integer arg)))
			  (:cut (cut cards (parse-integer arg)))))))
      (apply-instructions (cdr instructions) next-cards))
    cards))

(defun shuffle-cards (instructions &optional (number-of-cards +number-of-cards+))
  (apply-instructions instructions (iota number-of-cards)))

(defun solution-1 ()
  (let* ((input (read-input #p"day-22-input.txt"))
	 (shuffled (shuffle-cards input)))
    (position 2019 shuffled)))

;; --------------------------------- Part 2 ---------------------------------

;; https://codeforces.com/blog/entry/72593
;; each transformation can be rewritten in the form of 𝑓(𝑥) = 𝑎𝑥 + 𝑏 mod 𝑚
;; "deal into new stack": 𝑓(𝑥) = −𝑥−1  mod 𝑚, so 𝑎 = −1,𝑏 = −1
;; "cut 𝑛": 𝑓(𝑥) = 𝑥−𝑛  mod 𝑚, so 𝑎 = 1,𝑏 = −𝑛
;; "deal with increment 𝑛": 𝑓(𝑥) = 𝑛⋅𝑥  mod 𝑚, so 𝑎 = 𝑛,𝑏 = 0

;; (𝑎,𝑏) ;(𝑐,𝑑)=(𝑎𝑐 mod 𝑚,𝑏𝑐+𝑑  mod 𝑚)
(defun compose-lcf (lcf-1 lcf-2 m)
  (destructuring-bind (a . b) lcf-1
    (destructuring-bind (c . d) lcf-2
      (cons (mod (* a c) m) (mod (+ (* b c) d) m)))))

(defun instructions->lcfs (instructions &optional result)
  (if-let (instr (car instructions))
    (let ((lcf (destructuring-bind (fname . arg) instr
		 (ecase fname
		   (:deal-into-new (cons -1 -1))
		   (:deal-with-increment (cons (parse-integer arg) 0))
		   (:cut (cons 1 (- (parse-integer arg)) ))))))
      (instructions->lcfs (cdr instructions) (cons lcf result)))
    (nreverse result)))

(defconstant +number-of-cards-2+ 119315717514047)
(defconstant +number-of-shuffles+ 101741582076661)

(defun solution-2 ()
  (let* ((lcfs (instructions->lcfs (read-input #p"day-22-input.txt")))
	(lcf-0 (reduce (rcurry #'compose-lcf +number-of-cards-2+) lcfs))
	(lcf (pow-compose lcf-0 +number-of-shuffles+ +number-of-cards-2+)))
    (inverted-lcf lcf 2020 +number-of-cards-2+)))

(defun pow-compose (f0 k0 m)
  (iter
    (with g = (cons 1 0))
    (for k :initially k0 :then (truncate k 2))
    (for f :initially f0 :then (compose-lcf f f m))
    (while (plusp k))
    (when (oddp k)
      (setf g (compose-lcf g f m)))
    (finally
     (return g))))

(defun pow-mod (x0 n0 m)
  (iter
    (with y = 1)
    (for n :initially n0 :then (truncate n 2))
    (for x :initially x0 :then (mod (* x x) m))
    (while (plusp n))
    (when (oddp n)
      (setf y (mod (* y x) m)))
    (finally
     (return y))))

(defun invert (x m)
  (mod (pow-mod x (- m 2) m) m))

(defun inverted-lcf (lcf x m)
  (destructuring-bind (a . b) lcf
    (let ((inverse (invert a m)))
      (mod (* (- x b) inverse) m))))
