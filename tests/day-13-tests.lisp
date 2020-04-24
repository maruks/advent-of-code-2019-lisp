(defpackage :day-13-tests
  (:use :cl :cacau :assert-p :day-13))

(in-package :day-13-tests)

(deftest "solution-1 test" ()
  (eql-p 335 (solution-1)))

(deftest "solution-2 test" ()
  (eql-p 15706 (solution-2)))
