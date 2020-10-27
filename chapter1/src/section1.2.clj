(ns section1.2)

; Exercise 1.9

; This is a recursive process
(defn plus [a b]
  (if (= a 0)
    b
    (inc (plus (dec a) b))))

; This is an iterative process
; NB: Clojure requires use of 'recur' function to ensure tail-call optimization
(defn plus [a b]
  (if (= a 0)
    b
    (recur (dec a) (inc b))))


; Exercise 1.10: Ackermann's function

(defn A [x y]
  (cond
    (= y 0) 0
    (= x 0) (* 2 y)
    (= y 1) 2
    :else (A (- x 1) (A x (- y 1)))))

(A 1 10)
; => 2^10

(A 2 4)
; => 2^16

(A 3 3)
; => 2^16

(defn F [n]
  (A 0 n))
; => F(n) = 2n

(defn G [n]
  (A 1 n))
; => G(n) = 2^n

(defn H [n]
  (A 2 n))
; => H(n) is kind of broken. really it's H(n) = 2^(H(n-1)) except for H(0) = 0 and H(1) = 2
; So H(4) is 2^H(3) = 2^2^H(2) = 2^2^2^H(1) = 2^2^2^2 = 65536
; That means H(5) = 2^65536, and absurdly large number


(defn K [n]
  (* 5 n n))
; => K(n) = 5n^2


; Exercise 1.11

(defn f-recur [n]
  (if
      (< n 3) n
      (+ (f-recur (- n 1)) (* 2 (f-recur (- n 2))) (* 3 (f-recur (- n 3))))))

(defn f-iter-iter [sum sum2 sum3 count]
  (if
      (= count 2) sum
      (recur (+ sum (* 2 sum2) (* 3 sum3)) sum sum2 (dec count))))

(defn f-iter [n]
  (if
      (< n 3) n
      (f-iter-iter 2 1 0 n)))

; Exercise 1.12: Pascal's triangle
; P(i, j) denotes the jth element of row i


; P(i, 0) = 1
; P(k, k) = 1
; P(i, j) = P(i-1, j-1) + P(i-1, j)

(defn pascal [i j]
  (cond
    (= j 0) 1
    (= i j) 1
    (and (> j 0) (<= j i)) (+ (pascal (- i 1) (- j 1)) (pascal (- i 1) j))
    :else nil))

