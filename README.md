# Advent of Code 2019

### Test

    sbcl --non-interactive --eval "(ql:quickload :advent-of-code-2019/tests)" --eval "(asdf:test-system :advent-of-code-2019)"


	ccl -e "(ql:quickload :advent-of-code-2019/tests)" -e "(asdf:test-system :advent-of-code-2019)" -e "(quit)"

### Run

	sbcl --non-interactive --eval "(ql:quickload :advent-of-code-2019)" --eval "(main:main)"


	ccl -e "(ql:quickload :advent-of-code-2019)" -e "(main:main)" -e "(quit)"

### Build binary

	sbcl --non-interactive --eval "(ql:quickload :advent-of-code-2019)" --eval "(asdf:make :advent-of-code-2019)"


	ccl -e "(ql:quickload :advent-of-code-2019)" -e "(asdf:make :advent-of-code-2019)" -e "(ccl:save-application \"advent-of-code\" :toplevel-function #'main:main :prepend-kernel t)"


![Alt text](./aoc2019.png?raw=true "AOC 2019")

![Alt text](./made-with-lisp.png?raw=true "made with lisp")

* SBCL
* Clozure CL
