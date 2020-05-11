(defpackage :day-9-tests
  (:use :cl :cacau :assert-p :day-9)
    (:import-from :day-5 :run-program :allocate-program-memory :run-program-collect-results))

(in-package :day-9-tests)

(defsuite :day-9 ()

  (deftest "example-1" ()
    (let ((code '(109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99)))
      (equalp-p code
		(run-program-collect-results (allocate-program-memory (apply #'vector code) 200) '()))))

  (deftest "solution-1" ()
    (equalp-p '(3013554615) (solution-1)))

  (deftest "solution-2" ()
    (equalp-p '(50158) (solution-2))))
