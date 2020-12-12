(ns chapter2.section3)

; Exercise 2.53

; (list 'a 'b 'c)
; (a b c)

; (list (list 'george))
; ((george))

; (cdr '((x1 x2) (y1 y2))
; ((y1 y2))

; (cadr '((x1 x2) (y1 y2)))
; (y1 y2)

; (pair? (car '(a short list)))
; false

; (memq 'red '((red shoes) (blue socks)))
; false

; (memq 'red '(red shoes blue socks))
; (red shoes blue socks)

; Exercise 2.54
(defn equal? [a b]
  (cond
    (and (nil? a) (nil? b)) true
    (and (symbol? a) (symbol? b)) (= a b)
    (and (list? a) (list? b)) (and
                               (equal? (first a) (first b))
                               (equal? (next a) (next b)))
    :else false))

; Exercise 2.55
; (car ''abracadabra)
; Is equivalent to: (car '(quote abracadabra))
; Which passes the unevaluated list (quote abracadabra) to car,
; which returns the first element of the cons cell, the symbol "quote"

; Exercise 2.56
(def variable? symbol?)

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (= m1 1) m2
        (= m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)
        ))

(defn make-exponentiation [b e]
  (cond (=number? e 0) 1
        (=number? e 1) b
        :else (list '** b e)))

(defn sum? [x]
  (and (seq? x) (= (first x) '+)))
(defn addend [s]
  (nth s 1))
(defn augend [s]
  (nth s 2))

(defn product? [x]
  (and (seq? x) (= (first x) '*)))
(defn multiplier [p]
  (nth p 1))
(defn multiplicand [p]
  (nth p 2))

(defn exponentiation? [x]
  (and (seq? x) (= (first x) '**)))
(defn base [x]
  (nth x 1))
(defn exponent [x]
  (nth x 2))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum
                    (deriv (addend exp) var)
                    (deriv (augend exp) var))
        (product? exp) (make-sum
                        (make-product (multiplier exp)
                                      (deriv (multiplicand exp) var))
                        (make-product (deriv (multiplier exp) var)
                                      (multiplicand exp)))
        (exponentiation? exp) (make-product
                               (exponent exp)
                               (make-product
                                (make-exponentiation (base exp) (make-sum (exponent exp) -1))
                                (deriv (base exp) var)))
        :else (throw (str "unknown epxression type -- DERIV" exp))))

; Exercise 2.57
(defn make-sum [a1 a2]
  (cond
    (=number? a1 0) a2
    (=number? a2 0) a1
    (and (number? a1) (number? a2)) (+ a1 a2)
    (sum? a2) (concat (list '+ a1) (rest a2))
    (product? a2) (list '+ a1 a2)
    (exponentiation? a2) (list '+ a1 a2)
    (seq? a2) (concat (list '+ a1) a2)
    :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond
    (or (=number? m1 0) (=number? m2 0)) 0
    (= m1 1) m2
    (= m2 1) m1
    (and (number? m1) (number? m2)) (* m1 m2)
    (product? m2) (concat (list '* m1) (rest m2))
    (sum? m2) (list '* m1 m2)
    (exponentiation? m2) (list '* m1 m2)
    (seq? m2) (concat (list '* m1) m2)
    :else (list '* m1 m2)))

(defn augend [s]
  (let [terms (rest (rest s))]
    (cond
      (= (count terms) 1) (first terms)
      :else (make-sum (first terms) (rest terms)))))

(defn multiplicand [p]
  (let [terms (rest (rest p))]
    (cond
      (= (count terms) 1) (first terms)
      :else (make-product (first terms) (rest terms)))))
