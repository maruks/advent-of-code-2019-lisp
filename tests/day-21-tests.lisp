(defpackage #:day-21-tests
  (:use #:cl #:cacau #:assert-p #:day-21 #:alexandria))

(in-package #:day-21-tests)

(defsuite :day-21 ()

  (deftest "solution-1 test" ()
    (eql-p 19358262 (solution-1)))

  (deftest "solution-2 test" ()
    (eql-p 1142686742 (solution-2))))
