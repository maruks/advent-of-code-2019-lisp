(defpackage #:day-9-tests
  (:use #:cl #:cacau #:assert-p #:day-9))

(in-package #:day-9-tests)

(defsuite :day-9 ()

  (deftest "solution-1" ()
    (eql-p 3013554615 (solution-1)))

  (deftest "solution-2" ()
    (eql-p 50158 (solution-2))))
