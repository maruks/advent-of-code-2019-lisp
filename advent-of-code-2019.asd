;;;; advent-of-code-2019.asd

(defsystem "advent-of-code-2019"
  :description "https://adventofcode.com/2019"
  :author "Maris Orbidans"
  :license "Public license"
  :version "0.0.1"
  :serial t
  :depends-on (:iterate :split-sequence)
  :build-operation "program-op"
  :build-pathname "advent-of-code"
  :entry-point "main:main"

  :components ((:module "src"
		:components ((:file "advent-of-code")
			     (:file "day-1")
			     (:file "day-2")
			     (:file "day-3")
			     (:file "main")
			     )))
  :in-order-to ((test-op (test-op "advent-of-code-2019/tests"))))

(defsystem "advent-of-code-2019/tests"
  :license "Public license"
  :depends-on (:advent-of-code-2019
	       :cacau
	       :assert-p)
  :serial t
  :components ((:module "tests"
		:components ((:file "day-1-tests")
			     (:file "day-2-tests")
			     (:file "day-3-tests")
			     )))
  :perform (test-op (o c) (symbol-call 'cacau 'run :colorful t :reporter :list)))
