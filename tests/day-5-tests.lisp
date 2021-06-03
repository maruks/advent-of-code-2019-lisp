(defpackage #:day-5-tests
  (:use #:cl #:cacau #:alexandria #:assert-p #:day-5))

(in-package #:day-5-tests)

(defsuite :day-5 ()
  (deftest "solution-1" ()
    (eql-p 13294380 (solution-1)))
  (deftest "solution-2" ()
    (eql-p 11460760 (solution-2))))
