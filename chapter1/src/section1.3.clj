(ns section1.3)

; Exercise 1.29: Simpson's rule

; First we define numerical quadrature in terms of a weight function
; and a point function. The weight function should give the coefficient
; for point i, the point function should give the value of x for point i
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

; Simpson's rule point function depends on a b and n
(defn simp-x [a b n]
  (fn [i]
    (+ a (* i (/ (- b a) n)))))

; Finally we define Simpson's rule as a special case of quadrature
(defn simp [f a b n]
  (* (/ (- b a) (*  3 n)) (quad f (simp-w n) (simp-x a b n) n)))

(simp cube 0 1.0 100)
; 0.24338333333333334

(simp cube 0 1.0 1000)
; 0.24933383333333317

(simp cube 0 1.0 10000000)
; 0.24999993333333595
; Close enough!
