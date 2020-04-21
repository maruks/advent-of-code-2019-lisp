(defpackage :day-11-tests
  (:use :cl :cacau :assert-p :day-11 :advent-of-code))

(in-package :day-11-tests)

(deftest "solution-1 test" ()
  (eql-p 1909 (solution-1)))
