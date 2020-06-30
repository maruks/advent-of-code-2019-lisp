(defpackage :day-24-tests
  (:use :cl :cacau :assert-p :day-24))

(in-package :day-24-tests)

(defsuite :day-24 ()

  (deftest "solution-1 test" ()
    (eql-p 2130474 (solution-1)))

  (deftest "solution-2 test" ()
    (eql-p 1923 (solution-2))))
