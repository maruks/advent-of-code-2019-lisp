(defpackage #:day-25-tests
  (:use #:cl #:cacau #:assert-p #:day-25))

(in-package #:day-25-tests)

(defsuite :day-25 ()
  (deftest "solution-1 test" ()
    (string= "2622472" (solution-1))))
