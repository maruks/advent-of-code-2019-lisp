(defpackage :day-23-tests
  (:use :cl :cacau :assert-p :day-23))

(in-package :day-23-tests)

(defsuite :day-23 ()

  (deftest "solution-1 test" ()
    (eql-p 19530 (solution-1)))

  (deftest "solution-2 test" ()
    (eql-p 12725 (solution-2))))
