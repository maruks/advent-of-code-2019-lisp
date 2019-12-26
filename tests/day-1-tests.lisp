(defpackage :day-1-tests
  (:use :cl :cacau :assert-p :day-1))

(in-package :day-1-tests)

(deftest "fuel" ()
  (eql-p 2 (fuel 14))
  (eql-p 33583 (fuel 100756)))

(deftest "fuel-2" ()
  (eql-p 2 (fuel-2 14))
  (eql-p 50346 (fuel-2 100756)))
