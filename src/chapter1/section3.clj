(ns chapter1.section3)

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

(defn divides? [a b]
  (= (mod b a) 0))

(defn find-divisor [n test-divisor]
  (cond
    (> (* test-divisor test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (and (> n 1) (= n (smallest-divisor n))))

(defn sum-squared-primes [a b]
  (accumulate-filter + 0 square a inc b prime?))

(sum-squared-primes 1 10)
; 87

(defn gcd [a b]
  (if (= b 0) a
      (recur b (mod a b))))

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

; Exercise 1.34

(defn f [g]
  (g 2))

(f square)
; => 4

(f (fn [z] (* z (+ z 1))))
; => 6

; What if we ask for:
#_(f f)

; The interprefer first evaluates the argument 'f', which evaluates to a function (call this lambda)
; Then the interpreter applies the function f to lambda, which results in (lambda 2)
; However this cannot be evaluated, since lambda expects its argument to be a function, and instead it gets a number
; So we get a type error

; Exercise 1.35

; Show golden ratio (phi) is fixed point of f(x) = 1 + 1/x
; 1 + 1/x = x 
;   => x^2 - x - 1 = 0
;   => use quadratic formula to get x = (1 + sqrt(5))/2 as one of the roots

; Use fixed point function to compute golden ratio

(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (Math/abs (- v1 v2)) tolerance))
  (defn try-guess [guess]
    (let [next (f guess)]
      (if (close-enough? guess next)
        next
        (try-guess next))))
  (try-guess first-guess))

(fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0)
; => 1.6180327868852458

; Exercise 1.36

(defn fixed-point [f first-guess]
  (defn close-enough? [v1 v2]
    (< (Math/abs (- v1 v2)) tolerance))
  (defn try-guess [guess]
    (let [next (f guess)]
      (println next)
      (if (close-enough? guess next)
        next
        (try-guess next))))
  (try-guess first-guess))

(defn fixed-point-damped [f first-guess]
  (defn close-enough? [v1 v2]
    (< (Math/abs (- v1 v2)) tolerance))
  (defn try-guess-damped [guess]
    (let [next (/ (+ guess (f guess)) 2)]
      (println next)
      (if (close-enough? guess next)
        next
        (try-guess-damped next))))
  (try-guess-damped first-guess))

; (fixed-point #(/ (Math/log 1000) (Math/log %)) 4.5)
; 4.555539595243813
; Without damping: 23 steps

; (fixed-point-damped #(/ (Math/log 1000) (Math/log %)) 4.5)
; 4.555534397165625
; With damping: 5 steps

; Exercise 1.37
; a. As a recursive process
(defn cont-frac-recur [n d k]
  (defn iter [i]
    (if (= i (dec k))
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (iter (inc i))))))
  (iter 0))

(cont-frac-recur (constantly 1.0) (constantly 1.0) 11)
;  => 0.6180555555555556
; k = 11 is the smallest value that is correct to 4 decimal places

(defn cont-frac [n d k]
  (defn iter [result i]
    (if (< i 0)
      result
      (recur (/ (n i) (+ (d i) result)) (dec i))))
  (iter 0 (dec k)))

(cont-frac (constantly 1.0) (constantly 1.0) 11)
;  => 0.6180555555555556

; Exercise 1.38
(defn euler-d [i]
  (if (= 0 (mod (- i 1) 3))
    (* 2 (/ (+ i 2) 3))
    1))

(cont-frac (constantly 1.0) euler-d 100)
; => 0.7182818284590453

; Exercise 1.39
(defn tan-cf [x k]
  (defn n [i]
    (if (= i 0) x (* -1 (* x x))))
  (defn d [i] (+ (* 2 i) 1.0))
  (cont-frac n d k))

(tan-cf 1 100)
; => 1.557407724654902
