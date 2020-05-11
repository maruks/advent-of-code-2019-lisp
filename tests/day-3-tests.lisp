(defpackage :day-3-tests
  (:use :cl :cacau :assert-p :advent-of-code :day-3))

(in-package :day-3-tests)

(defsuite :day-3 ()
  (deftest "->points test" ()
    (equalp-p (list (make-point :x 0 :y 0) (make-point :x 0 :y -10) (make-point :x 5 :y -10) )
	      (->points '((:u . 10) (:r . 5)))))

  (deftest "intersect test" ()
    (equalp-p (cons 0 0) (intersect (make-point :x 0 :y 0) (make-point :x 0 :y 10) (make-point :x 0 :y 0) (make-point :x 10 :y 0)))
    (equalp-p (cons 0 0) (intersect (make-point :x 0 :y 0) (make-point :x 0 :y 10) (make-point :x 0 :y 0) (make-point :x -10 :y 0)))
    (equalp-p (cons 0 0) (intersect (make-point :x 0 :y 0) (make-point :x 0 :y -10) (make-point :x 0 :y 0) (make-point :x 10 :y 0)))
    (equalp-p (cons 0 0) (intersect (make-point :x 0 :y 0) (make-point :x 0 :y -10) (make-point :x 0 :y 0) (make-point :x -10 :y 0)))
    (equalp-p (cons 0 0) (intersect (make-point :x 0 :y 0) (make-point :x 10 :y 0) (make-point :x 0 :y 0) (make-point :x 0 :y 10)))
    (equalp-p (cons 0 0) (intersect (make-point :x 0 :y 0) (make-point :x -10 :y 0) (make-point :x 0 :y 0) (make-point :x 0 :y 10)))
    (equalp-p (cons 0 0) (intersect (make-point :x 0 :y 0) (make-point :x 10 :y 0) (make-point :x 0 :y 0) (make-point :x 0 :y -10)))
    (equalp-p (cons 0 0) (intersect (make-point :x 0 :y 0) (make-point :x -10 :y 0) (make-point :x 0 :y 0) (make-point :x 0 :y -10)))
    (equalp-p (cons 2 2) (intersect (make-point :x 2 :y 0) (make-point :x 2 :y 10) (make-point :x 0 :y 2) (make-point :x 10 :y 2)))
    (null-p (intersect (make-point :x 2 :y 10) (make-point :x 1 :y 10) (make-point :x 0 :y 2) (make-point :x 10 :y 2))))

  (deftest "closest intersection test" ()
    (eql-p 6 (closest-intersection
  	      '((:R . 8) (:U . 5) (:L . 5) (:D . 3) )
  	      '((:U . 7) (:R . 6) (:D . 4) (:L . 4))))
    (eql-p 159 (closest-intersection
  		'((:U . 62) (:R . 66) (:U . 55) (:R . 34) (:D . 71) (:R . 55) (:D . 58) (:R . 83))
  		'((:R . 75) (:D . 30) (:R . 83) (:U . 83) (:L . 12) (:D . 49) (:R . 71) (:U . 7) (:L . 72))))
    (eql-p 135 (closest-intersection
  		'((:R . 98) (:U . 47) (:R . 26) (:D . 63) (:R . 33) (:U . 87) (:L . 62) (:D . 20) (:R . 33) (:U . 53) (:R . 51))
  		'((:U . 98) (:R . 91) (:D . 20) (:R . 16) (:D . 67) (:R . 40) (:U . 7) (:R . 15) (:U . 6) (:R . 7)))))

  (deftest "lowest steps test" ()
    (eql-p 30 (lowest-steps
  	       '((:R . 8) (:U . 5) (:L . 5) (:D . 3) )
  	       '((:U . 7) (:R . 6) (:D . 4) (:L . 4))))
    (eql-p 610 (lowest-steps
  		'((:U . 62) (:R . 66) (:U . 55) (:R . 34) (:D . 71) (:R . 55) (:D . 58) (:R . 83))
  		'((:R . 75) (:D . 30) (:R . 83) (:U . 83) (:L . 12) (:D . 49) (:R . 71) (:U . 7) (:L . 72))))
    (eql-p 410 (lowest-steps
  		'((:R . 98) (:U . 47) (:R . 26) (:D . 63) (:R . 33) (:U . 87) (:L . 62) (:D . 20) (:R . 33) (:U . 53) (:R . 51))
  		'((:U . 98) (:R . 91) (:D . 20) (:R . 16) (:D . 67) (:R . 40) (:U . 7) (:R . 15) (:U . 6) (:R . 7))))))
