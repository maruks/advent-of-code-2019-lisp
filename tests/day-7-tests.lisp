(defpackage :day-7-tests
  (:use :cl :cacau :assert-p :day-7))

(in-package :day-7-tests)

(deftest "solution-1" ()
  (eql-p 206580 (solution-1)))

(deftest "solution-2" ()
  (eql-p 2299406 (solution-2)))
