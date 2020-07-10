(defpackage #:day-11-tests
  (:use #:cl #:cacau #:assert-p #:day-11 #:aoc))

(in-package #:day-11-tests)

(defsuite :day-11 ()

  (deftest "solution-1 test" ()
    (eql-p 1909 (solution-1))))
