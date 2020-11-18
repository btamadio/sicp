(ns chapter1.section3
  (:require [chapter1.section2 :refer [prime? gcd]]))

; Exercise 1.29: Simpson's rule

; First we define numerical quadrature in terms of a weight function
; and a point function. The weight function gives the coefficient
; for point i, the point function gives the value of x for point i
(defn quad [f weight-func point-func n]
  (let [term #(* (weight-func %) (f (point-func %)))]
    (reduce + (map term (range n)))))

; Simpson's rule weight function depends on n
(defn simp-w [n]
  (fn [i]
    (cond
      (= i 0) 1
      (= i n) 1
      (even? i) 4
      :else 2)))

; Simpson's rule point function depends on a, b, and n
(defn simp-x [a b n]
  (fn [i]
    (+ a (* i (/ (- b a) n)))))

; Finally we define Simpson's rule as a special case of quadrature
; Times and overall normalization factor
(defn simp [f a b n]
  (let [h (/ (- b a) n)]
    (* (/ h 3) (quad f (simp-w n) (simp-x a b n) n))))

(defn cube [x] (* x x x))

(simp cube 0 1.0 100)
; => 0.24338333333333334

(simp cube 0 1.0 1000)
; => 0.24933383333333317

(simp cube 0 1.0 10000000)
; => 0.24999993333333595
; Close enough!

; Exercise 1.30
; Recursive sum
(defn sum [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(sum identity 1 inc 10)
; => 55

(sum cube 1 inc 10)
; => 3025

; Exercise 1.31

; As a recursive process
(defn prod [term a next b]
  (if (> a b)
    1
    (* (term a)
       (prod term (next a) next b))))

; As an iterative process
(defn prod [term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(defn factorial [n]
  (prod identity 1 inc n))

(factorial 4)
; => 24

(factorial 10)
; => 3628800

(defn square [x]
  (* x x))

; We want the product of (x-1)*(x+1)/(x^2) from x=3 to n
(defn pi-prod [n]
  (let [term #(/ (* (inc %) (dec %)) (square %))
        next #(+ 2 %)]
    (* 4.0 (prod term 3 next n))))

(pi-prod 10000)
; => 3.1417497371492673

; Exercise 1.32

; As a recursive process
(defn accumulate-recur [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a)
       (accumulate-recur combiner null-value term (next a) next b))))

; As an iterative process
(defn accumulate [combiner null-value term a next b]
  (defn iter [a result]
    (if (> a b)
      result
      (recur (next a) (combiner result (term a)))))
  (iter a null-value))

(defn sum [term a next b]
  (accumulate + 0 term a next b))

(defn prod [term a next b]
  (accumulate * 1 term a next b))

(sum square 1 inc 7)
; => 140

(prod identity 1 inc 4)
; => 24

; Exercise 1.33

(defn accumulate-filter [combiner null-value term a next b filter-func]
  (defn iter [a result]
    (if (> a b)
      result
      (recur (next a) (if (filter-func a)
                        (combiner result (term a))
                        result))))
  (iter a null-value))

(defn square [x] (* x x))

(defn sum-squared-primes [a b]
  (accumulate-filter + 0 square a inc b prime?))

(sum-squared-primes 1 10)
; 87

(defn rel-prime [i n]
  (= (gcd i n) 1))

(defn prod-rel-primes [n]
  (defn rel-prime-n? [i]
    (rel-prime i n))
  (accumulate-filter * 1 identity 1 inc n rel-prime-n?))

(prod-rel-primes 5)
; 24

(prod-rel-primes 6)
; 5
