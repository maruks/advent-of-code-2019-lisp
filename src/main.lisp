(defpackage :main
  (:use :cl :uiop :uiop/stream)
  (:export :main))

(in-package :main)

(defun run ()
  (println "---------------------- day 1 ----------------------")
  (println (day-1:solution-1))
  (println (day-1:solution-2))
  (println "---------------------- day 2 ----------------------")
  (println (day-2:solution-1))
  (println (day-2:solution-2 19690720)))

(defun main ()
  (call-with-fatal-condition-handler #'run))
