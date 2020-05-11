(defpackage :day-12-tests
  (:use :cl :cacau :assert-p :day-12))

(in-package :day-12-tests)

(defsuite :day-12 ()

  (deftest "solution-1 test" ()
    (eql-p 13045 (solution-1)))

  (deftest "solution-2 test" ()
    (eql-p 344724687853944 (solution-2))))
