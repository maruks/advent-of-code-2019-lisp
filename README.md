# Advent of Code 2019

### Test

    ccl -e "(ql:quickload :advent-of-code-2019/tests)" -e "(asdf:test-system :advent-of-code-2019)" -e "(quit)"

### Run

	ccl -e "(ql:quickload :advent-of-code-2019)" -e "(main:main)" -e "(quit)"

### Build binary

	ccl -e "(ql:quickload :advent-of-code-2019)" -e "(asdf:make :advent-of-code-2019)" -e "(ccl:save-application \"advent-of-code\" :toplevel-function #'main:main :prepend-kernel t)"

![Alt text](./made-with-lisp.png?raw=true "AOC 2019")
