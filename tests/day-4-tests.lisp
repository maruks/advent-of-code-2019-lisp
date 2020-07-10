(defpackage #:day-4-tests
  (:use #:cl #:cacau #:assert-p #:aoc #:day-4))

(in-package #:day-4-tests)

(defsuite :day-4 ()

  (deftest "solution-1" ()
    (eql-p 1169 (solution-1)))

  (deftest "solution-2" ()
    (eql-p 757 (solution-2))))
