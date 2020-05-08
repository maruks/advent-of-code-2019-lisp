#!/bin/bash

ccl -e "(ql:quickload :advent-of-code-2019)" -e "(asdf:make :advent-of-code-2019)" -e "(ccl:save-application \"advent-of-code\" :toplevel-function #'main:main :prepend-kernel t)"
