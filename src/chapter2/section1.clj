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

; Exercise 2.7

(defn lower-bound [interval]
  (first interval))

(defn upper-bound [interval]
  (second interval))

(defn make-interval [a b]
  (pair a b))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(add-interval (make-interval 1 2) (make-interval 3 4))
; => [4 6]

; Exercise 2.8

(defn subtract-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(subtract-interval (make-interval 1.5 2.0) (make-interval 0.2 0.3))
; => [1.2 1.8]

; Exercise 2.9
; We can show that width of the sum of two intervals is equal to the sum of the widths of the intervals:
; width(sum(x,y)) = 0.5 * (upper(sum(x,y)) - lower(sum(x,y)))
;                 = 0.5 * (upper(x) + upper(y) - lower(x) - lower(y))
;                 = width(x) + width(y)

(defn width-interval [x]
  (/ (- (upper-bound x) (lower-bound x)) 2))

(width-interval (make-interval 0.5 3.0))
; => 1.25

(def int1 (make-interval 1.6 2.9))
(def int2 (make-interval 0.25 0.4))

; The width of the sum of intervals is the sum of widths of the intervals:
(width-interval (add-interval int1 int2))
; => 0.7249999999999999

(+ (width-interval int1) (width-interval int2))
; => 0.7249999999999999

(width-interval (mul-interval int1 int2))
; => 0.37999999999999995

; This is not true for multiplication.

; As a counterexample, consider intervals 3 and 4, which have the same widths as intervals 1 and 2
; The product of intervals 3 and 4 has a different width than the product of intervals 1 and 2
(def int3 (make-interval 0 1.3))
(def int4 (make-interval 0.15 0.3))

(width-interval int1)
; => 0.6499999999999999

(width-interval int2)
; => 0.07500000000000001

(width-interval int3)
; => 0.65

(width-interval int4)
; => 0.075

(width-interval (mul-interval int1 int2))
; => 0.37999999999999995

(width-interval (mul-interval int3 int4))
; => 0.195

; Exercise 2.10
(defn contains-zero? [interval]
  (and (<= (lower-bound interval) 0) (>= (upper-bound interval) 0)))

(defn div-interval [x y]
  (if (contains-zero? y)
    (throw (Exception. "division by zero"))
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

; Exercise 2.11
(defn pos-interval? [interval]
  (and (not (contains-zero? interval)) (pos? (upper-bound interval))))

(defn neg-interval? [interval]
  (and (not (contains-zero? interval)) (neg? (lower-bound interval))))

(defn mul-interval-new [x y]
  (cond

    (and (pos-interval? x) (pos-interval? y)) (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
    (and (pos-interval? x) (neg-interval? y)) (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y)))
    (and (pos-interval? x) (contains-zero? y)) (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
    (and (neg-interval? x) (pos-interval? y)) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
    (and (neg-interval? x) (neg-interval? y)) (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))
    (and (neg-interval? x) (contains-zero? y)) (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))
    (and (contains-zero? x) (pos-interval? y)) (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y)))
    (and (contains-zero? x) (neg-interval? y)) (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y)))
    (and (contains-zero? x) (contains-zero? y)) (mul-interval x y)))

; Exercise 2.12

(defn make-center-percent [c p]
  (make-interval (- c (* p c)) (+ c (* p c))))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn percent [i]
  (/ (width i) (center i)))

; Exercise 2.13

; With some algebra, one can show that for small percent tolerances:
; (percent (mul-interval x y)) is approximately (+ (percent x) (percent y))

; We can check with some test cases

(def int-test1 (make-center-percent 2.0 0.01))
(def int-test2 (make-center-percent 4.5 0.05))

(percent (mul-interval int-test1 int-test2))
; => 0.05997001499250365

(+ (percent int-test1) (percent int-test2))
; => 0.05999999999999993

; Exercise 2.14

(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval
                   (div-interval one r1)
                   (div-interval one r2)))))

(par1 int1 int2)
; => [0.12121212121212122 0.6270270270270268]

(par2 int1 int2)
; => [0.21621621621621623 0.3515151515151515]

(def inta (make-center-percent 4 0.04))
(def intb (make-center-percent 2 0.01))

(div-interval inta inta)
; => [0.9230769230769229 1.0833333333333335]

(div-interval inta intb)
; => [1.900990099009901 2.101010101010101]

; Dividing an interval by itself doesn't give 1! 
; This is because the lower bound of the result is taking the lower bound of inta
; in the numerator and the upper bound of inta in the denominator.
; This is incorrect because it is treating the two occurrences of inta as independent,
; when in reality, the ratio A/A will always be 1 for any given measurement A

; Exercise 2.15
; Yes, Alyssa is solving the "dependency problem" that arises
; when an interval occurs more than once in a formula and the occurrences are treated independently.
; For example, the lower bound of par1 corresponds to R1 being at its lower bound in the numerator
; but simulatenously at its upper bound in the denominator (and likewise for R2). Since R1 is in reality one measurement,
; treating its two occurences as independent will overestimate the resulting interval.
; par2 is a "better" program because it does not suffer from this deficiency.


; Exercise 2.16
; Already answered the first part of this question as my answer to 2.15
; According to some quick Googling, there are some methods for solving this problem automatically
; but it is not always possible.

