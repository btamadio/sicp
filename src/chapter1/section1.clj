(ns chapter1.section1)

; Exercise 1.1

10 
; 10

(+ 5 3 4) 
; 12

(- 9 1)
; 8

(/ 6 2)
; 3

(+ (* 2 4) (- 4 6))
; 6

(def a 3)

(def b (+ a 1))

(+ a b (* a b))
; 19

(= a b)
; false

(if (and (> b a) (< b (* a b)))
  b
  a)
; 4

(cond
  (= a 4) 6
  (= b 4) (+ 6 7 a)
  :else 25)
; 16

(+ 2 (if (> b a) b a))
; 6

(* (cond
     (> a b) a
     (< a b) b
     :else -1)
   (+ a 1))
; 16

; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3
(defn sum-squares-largest-two
  [a b c]
  (cond
    (= (min a b c) a) (+ (* b b) (* c c))
    (= (min a b c) b) (+ (* a a) (* c c))
    :else (+ (* b b) (* c c))))

; Exercise 1.4
(defn a-plus-abs-b
  [a b]
  ((if (> b 0) + -) a b))
; if b is positive, applies '+' function to a and b, else applies '-' function

; Exercise 1.5
; This code won't compile in Clojure, but in Scheme the answer is:
; normal order: 0, because after fully expanding, (= x 0) evaulates to true, so the if statement only return 0 without evaluating y
; applicative order: infinite regress, because p cannot be evaluated

; (def p (p))
; (defn test [x y] (if (= x 0) 0 y))
; (test 0 (p))

; Exercise 1.6
(defn new-if [predicate then-clause else-clause]
  (cond predicate then-clause
        :else else-clause))

(defn good-enough? [guess x]
  (< (Math/abs (- (* guess guess) x)) 0.001))
(defn average [x y] (/ (+ x y) 2))
(defn improve [guess x] (average guess (/ x guess)))
(defn sqrt-iter [guess x]
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 1.0 x))

;(sqrt 9) gives StackOverflowError because both branches of conditional are evalauted.

; Exercise 1.7

; For very small numbers, 0.001 is a large fractional error
; say if the true result is 0.0000001 being off by 0.001 is a huge mistake

; For very large numbers, it may take a very long time to converge to within 0.001
; Since this could represent a tiny fraction of the actual result

; Solution
(defn good-enough? [prev-guess guess]
  (< (Math/abs (/ (- prev-guess guess) guess)) 0.001))

(defn sqrt-iter [prev-guess guess x]
  (if (good-enough? prev-guess guess)
    guess
    (sqrt-iter guess (improve guess x) x)))

(defn sqrt [x]
  (sqrt-iter 0.0 1.0 x))

; Exercise 1.8

(defn improve-cube [guess x]
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(defn cube-root-iter [prev-guess guess x]
  (if (good-enough? prev-guess guess)
    guess
    (cube-root-iter guess (improve-cube guess x) x)))

(defn cube-root [x]
  (cube-root-iter 0.0 1.0 x))
