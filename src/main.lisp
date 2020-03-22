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
  (println (day-2:solution-2 19690720))
  (println "---------------------- day 3 ----------------------")
  (println (day-3:solution-1))
  (println (day-3:solution-2))
  (println "---------------------- day 4 ----------------------")
  (println (day-4:solution-1 236491 713787))
  (println (day-4:solution-2 236491 713787))
  )

(defun main ()
  (call-with-fatal-condition-handler #'run))
