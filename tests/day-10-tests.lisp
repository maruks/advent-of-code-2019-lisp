(defpackage :day-10-tests
  (:use :cl :cacau :assert-p :day-10 :advent-of-code))

(in-package :day-10-tests)

(defparameter *example-0*
  '(".#..#"
    "....."
    "#####"
    "....#"
    "...##"))

(defparameter *example-1*
  '("......#.#."
    "#..#.#...."
    "..#######."
    ".#.#.###.."
    ".#..#....."
    "..#....#.#"
    "#..#....#."
    ".##.#..###"
    "##...#..#."
    ".#....####"))

(defparameter *example-2*
  '("#.#...#.#."
    ".###....#."
    ".#....#..."
    "##.#.#.#.#"
    "....#.#.#."
    ".##..###.#"
    "..#...##.."
    "..##....##"
    "......#..."
    ".####.###."))

(defparameter *example-3*
  '(".#..#..###"
    "####.###.#"
    "....###.#."
    "..###.##.#"
    "##.##.#.#."
    "....###..#"
    "..#.#..#.#"
    "#..#.#.###"
    ".##...##.#"
    ".....#.#.."))

(defparameter *example-4*
  '(".#..##.###...#######"
    "##.############..##."
    ".#.######.########.#"
    ".###.#######.####.#."
    "#####.##.#.##.###.##"
    "..#####..#.#########"
    "####################"
    "#.####....###.#.#.##"
    "##.#################"
    "#####.##.###..####.."
    "..######..##.#######"
    "####.##.####...##..#"
    ".#####..#.######.###"
    "##...#.##########..."
    "#.##########.#######"
    ".####.#.###.###.#.##"
    "....##.##.###..#####"
    ".#.#.###########.###"
    "#.#.#.#####.####.###"
    "###.##.####.##.#..##"))

(deftest "visible-points test 0" ()
  (let ((map (read-map *example-0*)))
    (eql-p 8 (visible-points (make-point :x 3 :y 4) map))
    (eql-p 7 (visible-points (make-point :x 2 :y 2) map))
    (eql-p 7 (visible-points (make-point :x 4 :y 0) map))
    (eql-p 5 (visible-points (make-point :x 4 :y 2) map))))

(deftest "visible-points test 1" ()
  (let ((map (read-map *example-1*)))
    (eql-p 33 (visible-points (make-point :x 5 :y 8) map))))

(deftest "visible-points test 2" ()
  (let ((map (read-map *example-2*)))
    (eql-p 35 (visible-points (make-point :x 1 :y 2) map))))

(deftest "visible-points test 3" ()
  (let ((map (read-map *example-3*)))
    (eql-p 41 (visible-points (make-point :x 6 :y 3) map))))

(deftest "visible-points test 4" ()
  (let ((map (read-map *example-4*)))
    (eql-p 210 (visible-points (make-point :x 11 :y 13) map))))

(deftest "best-location-point test" ()
  (equalp-p (list 8 (make-point :x 3 :y 4)) (best-location-point (read-map *example-0*)))
  (equalp-p (list 33 (make-point :x 5 :y 8)) (best-location-point (read-map *example-1*)))
  (equalp-p (list 35 (make-point :x 1 :y 2)) (best-location-point (read-map *example-2*)))
  (equalp-p (list 41 (make-point :x 6 :y 3)) (best-location-point (read-map *example-3*)))
  (equalp-p (list 210 (make-point :x 11 :y 13)) (best-location-point (read-map *example-4*))))

(deftest "solution-1 test" ()
  (eql-p 309 (solution-1)))

(deftest "solution-2 test" ()
  (equalp-p (make-point :x 4 :y 16) (solution-2)))
