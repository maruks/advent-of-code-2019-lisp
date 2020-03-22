(defpackage :day-4-tests
  (:use :cl :cacau :assert-p :advent-of-code :day-4))

(in-package :day-4-tests)

(deftest "solution-1" ()
  (eql-p 1169 (solution-1 236491 713787)))


(deftest "solution-2" ()
  (eql-p 757 (solution-2 236491 713787)))
