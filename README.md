# Advent of Code 2019

### SBCL

#### Test

    sbcl --non-interactive --eval "(ql:quickload :advent-of-code-2019/tests)" --eval "(asdf:test-system :advent-of-code-2019)"

#### Run

	sbcl --non-interactive --eval "(ql:quickload :advent-of-code-2019)" --eval "(main:main)"

#### Build binary

	sbcl --non-interactive --eval "(ql:quickload :advent-of-code-2019)" --eval "(asdf:make :advent-of-code-2019)"

### Clozure CL

#### Test

	ccl -e "(ql:quickload :advent-of-code-2019/tests)" -e "(asdf:test-system :advent-of-code-2019)" -e "(quit)"

#### Run

	ccl -e "(ql:quickload :advent-of-code-2019)" -e "(main:main)" -e "(quit)"

#### Build binary

	ccl -e "(ql:quickload :advent-of-code-2019)" -e "(asdf:make :advent-of-code-2019)" -e "(ccl:save-application \"advent-of-code\" :toplevel-function #'main:main :prepend-kernel t)"


![Alt text](./aoc2019.png?raw=true "AOC 2019")

![Alt text](./made-with-lisp.png?raw=true "made with lisp")
