(defpackage :day-19-tests
  (:use :cl :cacau :assert-p :day-19 :alexandria))

(in-package :day-19-tests)

(defsuite :day-19 ()

  (deftest "solution-1 test" ()
    (eql-p 162 (solution-1))))
