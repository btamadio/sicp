(ns chapter2.section1)

; Clojure doesn't have primitive cons cells or car/cdr
; Instead we can represent a pair as a vector
; and use first/second in place of car/cdr

(defn pair [a b]
  [a b])

; Exercise 2.1
(defn gcd [a b]
  (if (= b 0) a
      (recur b (mod a b))))

(defn make-rat [n d]
  (let [numer (Math/abs n)
        denom (Math/abs d)
        g (gcd numer denom)
        sign (if (> (* n d) 0) 1 -1)]
    (pair (* sign (/ numer g)) (/ denom g))))

; Exercise 2.2
(defn make-segment [start end]
  (pair start end))

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (second segment))

(defn make-point [x y]
  (pair x y))

(defn x-point [point]
  (first point))

(defn y-point [point]
  (second point))

(defn midpoint-segment [segment]
  (make-point
   (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2.0)
   (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2.0)))

; Exercise 2.3

; We'll need this function to compute the length of a segment
(defn len-segment [segment]
  (Math/sqrt (+ (square (- (y-point (end-segment segment)) (y-point (start-segment segment))))
                (square (- (x-point (start-segment segment)) (x-point (end-segment segment)))))))

; First representation: base (segment), height (number):
(defn make-rect-1 [base height]
  "base is a segment, height is a number"
  (pair base height))

(defn square [x] (* x x))

(defn height-rect [rect]
  (second rect))

(defn width-rect [rect]
  (len-segment (first rect)))

(defn area-rect [rect]
  (* (width-rect rect) (height-rect rect)))

(defn perim-rect [rect]
  (* 2 (+ (width-rect rect) (height-rect rect))))

(def r1 (make-rect-1 (make-segment (make-point 0 0) (make-point 3 0)) 4))

(area-rect r1)
; => 12.0

(perim-rect r1)
; => 14.0

; Second representation: diagonal (segment), angle between diagonals (in radians):
(defn make-rect-2 [diag angle]
  "diag is a segment, angle is a number of radians"
  (pair diag angle))

(defn height-rect [rect]
  (* (len-segment (first rect)) (Math/cos (/ (second rect) 2))))

(defn width-rect [rect]
  (* (len-segment (first rect)) (Math/sin (/ (second rect) 2))))

; With a little reverse engineering, we can define the same rectangle in this new representation:
(def r2 (make-rect-2 (make-segment (make-point 0 0) (make-point 3 4)) (* 2 (Math/asin (/ 4 5.0)))))

(area-rect r1)
; => 12.0

(perim-rect r1)
; => 14.0

