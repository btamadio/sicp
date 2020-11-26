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

; We'll need the len-segment function for computing perimeter and area
(defn square [x] (* x x))

(defn len-segment [segment]
  (Math/sqrt (+ (square (- (y-point (end-segment segment)) (y-point (start-segment segment))))
                (square (- (x-point (start-segment segment)) (x-point (end-segment segment)))))))

; First representation: base (segment), height (number):
(defn make-rect-1 [base height]
  "base is a segment, height is a number"
  (pair base height))



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

; Exercise 2.4

; (defn cons [x y]
;   (fn [m] (m x y)))

; (defn car [z]
;   (z (fn [p q] p)))

; Using substitution model:
; (car (cons a b))
; (car (fn [m] (m a b)))
; ((fn [m] (m a b)) (fn [p q] p))
; ((fn [p q] p) a b)
; a
; So, (car (cons a b)) => a, as required

; Definition of cdr in this representation:
; (defn cdr [z] (z (fn [p q] q)))

; Let's verify programatically as well:
(defn cons-proc [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [p q] p)))

(defn cdr [z]
  (z (fn [p q] q)))

(car (cons-proc 1 2))
; => 1

(cdr (cons-proc 1 2))
; => 2

; Exercise 2.5

(defn cons-exp [x y]
  (* (Math/pow 2 x) (Math/pow 3 y)))

(defn factorize [z p]
  (defn fact-iter [z p res]
    (if (not= 0 (int (mod z p)))
      res
      (recur (/ z p) p (inc res))))
  (fact-iter z p 0))

(defn car-exp [z]
  (factorize z 2))

(defn cdr-exp [z]
  (factorize z 3))

(car-exp (cons-exp 3 5))
; => 3

(cdr-exp (cons-exp 3 5))
; => 5

(car-exp (cons-exp 12 14))
; => 12

(cdr-exp (cons-exp 12 14))
; => 14

; Exercise 2.6

(def zero
  (fn [f]
    (fn [x] x)))

(defn add-1 [n]
  (fn [f]
    (fn [x] (f ((n f) x)))))

; Evaluate (add-1 zero) with substitution method:
; (add-1 zero)
; (fn [f] (fn [x] (f ((zero f) x))))
; (fn [f] (fn [x] (f (((fn [f] (fn [x] x)) f) x))))

; (fn [f] (fn [x] (f ((fn [x] x) x))))
; (fn [f] (fn [x] (f ((fn [x] x) x))))
; (fn [f] (fn [x] (f x)))

; So (add-1 zero) gives a function that applies a function f to x once

; We can check this by passing the successor function and and 0:
(((add-1 zero) inc) 0)
; => 1

; The number 2 is represented by applying a function twice:
(def two
  (fn [f]
    (fn [x]
      (f (f x)))))

((two inc) 0)
; => 2

; (plus m n) should then mean applying the function m+n times
(defn plus [m n]
  (fn [f]
    (fn [x]
      ((m f) ((n f) x)))))

; Use currying to make it a function of one argument, consistent with Lambda calculus:
(defn plus [m]
  (fn [n]
    (fn [f]
      (fn [x]
        ((m f) ((n f) x))))))

(def one (add-1 zero))
(def two (add-1 (add-1 zero)))

((((plus one) two) inc) 0)
; => 3

((((plus one) one) inc) 0)
; => 2

(def four ((plus two) two))
((four inc) 0)
; => 4

(def six ((plus four) two))
((six inc) 0)
; => 6
