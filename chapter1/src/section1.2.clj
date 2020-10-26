(ns section1.2)

; Exercise 1.9

; This is a recursive process, defined as a recursive procedure
(defn plus [a b]
  (if (= a 0)
    b
    (inc (plus (dec a) b))))

; This is an iterative process, defined as a recursive procedure
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
