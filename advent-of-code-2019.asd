;;;; advent-of-code-2019.asd

(defsystem "advent-of-code-2019"
  :description "https://adventofcode.com/2019"
  :author "Maris Orbidans"
  :license "Public license"
  :version "0.0.1"
  :serial t
  :depends-on (:iterate :split-sequence :alexandria :zpng)
  :build-operation "program-op"
  :build-pathname "advent-of-code"
  :entry-point "main:main"

  :components ((:module "src"
		:components ((:file "advent-of-code")
			     (:file "day-1")
			     (:file "day-2")
			     (:file "day-3")
			     (:file "day-4")
			     (:file "day-5")
			     (:file "day-6")
			     (:file "day-7")
			     (:file "day-8")
			     (:file "day-9")
			     (:file "day-10")
			     (:file "day-11")
			     (:file "main")
			     )))
  :in-order-to ((test-op (test-op "advent-of-code-2019/tests"))))

(defsystem "advent-of-code-2019/tests"
  :license "Public license"
  :depends-on (:advent-of-code-2019
	       :cacau
	       :alexandria
	       :assert-p)
  :serial t
  :components ((:module "tests"
		:components ((:file "day-1-tests")
			     (:file "day-2-tests")
			     (:file "day-3-tests")
			     (:file "day-4-tests")
			     (:file "day-5-tests")
			     (:file "day-6-tests")
			     (:file "day-7-tests")
			     (:file "day-8-tests")
			     (:file "day-9-tests")
			     (:file "day-10-tests")
			     (:file "day-11-tests")
			     )))
  :perform (test-op (o c) (symbol-call 'cacau 'run :colorful t :reporter :list)))
