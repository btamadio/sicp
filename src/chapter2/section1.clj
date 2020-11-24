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
        multiplier (if (> (* n d) 0) 1 -1)]
    (pair (* multiplier (/ numer g)) (/ denom g))))
