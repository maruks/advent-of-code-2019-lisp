(defpackage :day-15-tests
  (:use :cl :cacau :assert-p :day-15 :aoc))

(in-package :day-15-tests)

(defsuite :day-15 ()

  (deftest "solution-1 test" ()
    (equalp-p (cons 270 (make-point :x -20 :y 18)) (solution-1)))

  (deftest "solution-2 test" ()
    (eql-p 364 (solution-2))))
