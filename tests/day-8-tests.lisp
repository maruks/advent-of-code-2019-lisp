(defpackage :day-8-tests
  (:use :cl :cacau :assert-p :day-8))

(in-package :day-8-tests)

(deftest "solution-1" ()
  (eql-p 2048 (solution-1)))
