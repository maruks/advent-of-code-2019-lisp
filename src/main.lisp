(defpackage :main
  (:use :cl :uiop :uiop/stream)
  (:export :main))

(in-package :main)

(defmacro day (number &optional name)
  (flet ((solution (n)
	   (find-symbol (format nil "SOLUTION-~a" n) (format nil "DAY-~a" number) ))
	 (label (n)
	   (format nil "--- Day ~a~a ---" n (if name (concatenate 'string ": " name) ""))))
    `(progn
       (println ,(label number))
       (println (,(solution 1)))
       (println (,(solution 2))))))

(defun run ()
  (day 1 "The Tyranny of the Rocket Equation")
  (day 2 "1202 Program Alarm")
  (day 3 "Crossed Wires")
  (day 4 "Secure Container")
  (day 5 "Sunny with a Chance of Asteroids")
  (day 6 "Universal Orbit Map")
  (day 7 "Amplification Circuit")
  (day 8 "Space Image Format")
  (day 9 "Sensor Boost")
  (day 10 "Monitoring Station")
  (day 11 "Space Police")
  (day 12 "The N-Body Problem")
  (day 13 "Care Package")
  (day 14 "Space Stoichiometry")
  (day 15 "Oxygen System")
  (day 16 "Flawed Frequency Transmission")
  (day 17 "Set and Forget")
  (day 18 "Many-Worlds Interpretation")
  )

(defun main ()
  (call-with-fatal-condition-handler #'run))
