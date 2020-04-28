(defpackage :day-17-tests
  (:use :cl :cacau :assert-p :day-17 :alexandria))

(in-package :day-17-tests)

(deftest "solution-1 test" ()
  (eql-p 4044 (solution-1)))
