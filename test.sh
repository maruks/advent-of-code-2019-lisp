#!/bin/bash

ccl -e "(ql:quickload :advent-of-code-2019/tests)" -e "(asdf:test-system :advent-of-code-2019)" -e "(quit)"
