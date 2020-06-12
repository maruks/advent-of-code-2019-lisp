(defpackage :day-22-tests
  (:use :cl :cacau :assert-p :day-22))

(in-package :day-22-tests)

(defsuite :day-22 ()

  (deftest "solution-1 test" ()
    (eql-p 6289 (solution-1)))

  (deftest "solution-2 test" ()
    (eql-p 58348342289943 (solution-2))))
