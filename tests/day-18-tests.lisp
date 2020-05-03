(defpackage :day-18-tests
  (:use :cl :cacau :assert-p :day-18 :alexandria))

(in-package :day-18-tests)

(defparameter *example-1*
  '("#########"
    "#b.A.@.a#"
    "#########"))

(defparameter *example-2*
  '("########################"
    "#f.D.E.e.C.b.A.@.a.B.c.#"
    "######################.#"
    "#d.....................#"
    "########################"))

(defparameter *example-3*
  '("########################"
    "#...............b.C.D.f#"
    "#.######################"
    "#.....@.a.B.c.d.A.e.F.g#"
    "########################"))

(defparameter *example-4*
  '("#################"
    "#i.G..c...e..H.p#"
    "########.########"
    "#j.A..b...f..D.o#"
    "########@########"
    "#k.E..a...g..B.n#"
    "########.########"
    "#l.F..d...h..C.m#"
    "#################"))

(defparameter *example-5*
  '("########################"
    "#@..............ac.GI.b#"
    "###d#e#f################"
    "###A#B#C################"
    "###g#h#i################"
    "########################"))

(defparameter *example-6*
  '("#############"
    "#DcBa.#.GhKl#"
    "#.###@#@#I###"
    "#e#d#####j#k#"
    "###C#@#@###J#"
    "#fEbA.#.FgHi#"
    "#############"))

(defparameter *example-7*
  '("#############"
    "#g#f.D#..h#l#"
    "#F###e#E###.#"
    "#dCba@#@BcIJ#"
    "#############"
    "#nK.L@#@G...#"
    "#M###N#H###.#"
    "#o#m..#i#jk.#"
    "#############"))

(deftest "shortest-distance test" ()
  (eql-p 8 (shortest-distance *example-1*))
  (eql-p 86 (shortest-distance *example-2*))
  (eql-p 132 (shortest-distance *example-3*))
  (eql-p 136 (shortest-distance *example-4*))
  (eql-p 81 (shortest-distance *example-5*)))

;; (deftest "solution-1 test" ()
;;   (eql-p 2796 (solution-1)))
