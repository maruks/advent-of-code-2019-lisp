(defpackage :day-2-tests
  (:use :cl :cacau :assert-p :day-2))

(in-package :day-2-tests)

(deftest "run-intcode" ()
  (equalp-p (run-intcode (vector 1 1 1 4 99 5 6 0 99)) (vector 30 1 1 4 2 5 6 0 99))
  (equalp-p (run-intcode (vector 2 4 4 5 99 0)) (vector 2 4 4 5 99 9801))
  (equalp-p (run-intcode (vector 1 0 0 0 99)) (vector 2 0 0 0 99)))
