(defpackage :day-6-tests
  (:use :cl :cacau :assert-p :day-6))

(in-package :day-6-tests)

(deftest "solution-1" ()
  (eql-p 273985 (solution-1)))

(deftest "solution-2" ()
  (eql-p 460 (solution-2)))
