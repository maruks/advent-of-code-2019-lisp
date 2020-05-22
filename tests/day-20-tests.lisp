(defpackage :day-20-tests
  (:use :cl :cacau :assert-p :day-20 :alexandria))

(in-package :day-20-tests)

(defsuite :day-20 ()

  (deftest "shortest-path-length test" ()
    (eql-p 23 (shortest-path-length (read-input #p"day-20-example-1.txt")))
    (eql-p 58 (shortest-path-length (read-input #p"day-20-example-2.txt"))))

  (deftest "solution-1 test" ()
    (eql-p 666 (solution-1))))
