(defpackage #:day-5-tests
  (:use #:cl #:cacau #:alexandria #:assert-p #:day-5))

(in-package #:day-5-tests)

(defparameter *example-1* #(3 9 8 9 10 9 4 9 99 -1 8))
(defparameter *example-2* #(3 9 7 9 10 9 4 9 99 -1 8))
(defparameter *example-3* #(3 3 1108 -1 8 3 4 3 99))
(defparameter *example-4* #(3 3 1107 -1 8 3 4 3 99))
(defparameter *example-5* #(3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9))
(defparameter *example-6* #(3 3 1105 -1 9 1101 0 0 12 4 12 99 1))
(defparameter *example-7* #(3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99))

(defun test-program (program input)
  (run-program (copy-array program) (list input)))

(defsuite :day-5 ()

  (deftest "decode" ()
    (equal-p '(2 (0 1 0)) (multiple-value-list (decode 1002))))

  (deftest "solution-1" ()
    (eql-p 13294380 (solution-1)))

  (deftest "solution-2" ()
    (eql-p 11460760 (solution-2)))

  (deftest "run-program" ()
    (eql-p 1 (test-program *example-1* 8))
    (eql-p 0 (test-program *example-1* 7))
    (eql-p 0 (test-program *example-1* 9))

    (eql-p 1 (test-program *example-2* 7))
    (eql-p 0 (test-program *example-2* 8))
    (eql-p 0 (test-program *example-2* 9))

    (eql-p 1 (test-program *example-3* 8))
    (eql-p 0 (test-program *example-3* 7))
    (eql-p 0 (test-program *example-3* 9))

    (eql-p 1 (test-program *example-4* 7))
    (eql-p 0 (test-program *example-4* 8))
    (eql-p 0 (test-program *example-4* 9))

    (eql-p 0 (test-program *example-5* 0))
    (eql-p 1 (test-program *example-5* 5))
    (eql-p 1 (test-program *example-5* -2))

    (eql-p 0 (test-program *example-6* 0))
    (eql-p 1 (test-program *example-6* 5))
    (eql-p 1 (test-program *example-6* -2))

    (eql-p 999 (test-program *example-7* 7))
    (eql-p 1000 (test-program *example-7* 8))
    (eql-p 1001 (test-program *example-7* 10))))
