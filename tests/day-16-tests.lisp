(defpackage :day-16-tests
  (:use :cl :cacau :assert-p :day-16 :alexandria))

(in-package :day-16-tests)

(defsuite :day-16 ()

  (deftest "pattern-number test" ()
    (equal-p '(0 1 1 0 0 -1 -1 0 0 1 1 0 0 -1 -1) (mapcar (curry #'pattern-number 1) (iota 15))))

  (deftest "process-all test" ()
    (equal-p '(0 1 0 2 9 4 9 8) (process-all '(1 2 3 4 5 6 7 8) 4)))

  (deftest "solution-1 test" ()
    (eql-p  18933364 (solution-1)))

  (deftest "solution-2 test" ()
    (eql-p 28872305 (solution-2))))
