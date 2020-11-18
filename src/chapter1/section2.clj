(ns chapter1.section2)

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
    (and (> j 0) (<= j i)) (+ (pascal (- i 1) (- j 1)) (pascal (- i 1) j))))

; Exercise 1.13

; Exercise 1.14

(def first-denom {1 1 2 5 3 10 4 25 5 50})

(defn cc [amount kinds-of-coins]
  (cond
    (= amount 0) 1
    (or (< amount 0) (= kinds-of-coins 0)) 0
    :else (+ (cc amount (- kinds-of-coins 1))
             (cc (- amount (first-denom kinds-of-coins)) kinds-of-coins))))

(defn count-change [amount]
  (cc amount 5))

(count-change 100) ; => 292

(count-change 11) ; => 4
; Tree Diagram
; Omitting many nodes that evaluate to zero for simplicity
; 
;                                           (11 5)
;                                            /
;                                        (11 4)
;                                         /
;                                  __ (11 3)____________________
;                                /                              \
;                               /                               \
;              ___________(11 2)_____                         (1 3)
;             /                      \                         /
;            /                       \                        /
;       (11 1)                   ___(6 2)_____             (1 2)
;     /      \                  /             \             /
; (11 0)    (10 1)          (6 1)            (1 2)       (1 1)
;   |      /     \          /   \             /          /   \
;   0  (10 0)    (9 1)  (6 0)   (5 1)       (1 1)      (1 0) (0 1)
;        |      /    \    |    /    \        /  \        |     |
;        0   (9 0) (8 1)  0  (5 0) (4 1)  (1 0) (0 1)    0     1
;              |     |         |     |      |     |
;              0   (7 1)       0   (3 1)    0     1
;                   ...             ...
;                    1               1

; Exercise 1.15
(defn cube [x]
  (* x x x))

(defn p [x]
  (- (* 3 x) (* 4 (cube x))))

(defn sine [angle]
  (if (<= (Math/abs angle) 0.1)
    angle
    (p (sine (/ angle 3.0)))))

; How many times is function p applied when (sine 12.15) is evaluated?
; (sine 12.15)
;  = (p (sine 4.05))
;  = (p (p (sine 1.35)))
;  = (p (p (p (sine 0.45))))
;  = (p (p (p (p (sine 0.15)))))
;  = (p (p (p (p (p (sine 0.05))))))
;  = (p (p (p (p (p 0.05)))))
;   p is applied 5 times

(p (p (p (p (p 0.05)))))
;=> -0.39980345741334

(sine 12.15)
;=> -0.39980345741334


; Order of growth
;   a             num steps (applying function p)
;  <= 0.1            0
; 0.1 < a <= 0.3     1
; 0.3 < a <= 0.9     2
; 0.9 < a <= 2.7     3
; 2.7 < a <= 8.1     4
; 8.1 < a <= 24.3    5

; a ~ 0.1 * 3^n
; n ~ log(a)

(defn expt-iter [b counter product]
  (if (= counter 0)
    product
    (expt-iter b
               (- counter 1)
               (* b product))))

(defn expt [b n]
  (expt-iter b n 1))

(defn square [x] (* x x))

(defn fast-expt [b n]
  (cond
    (= n 0) 1
    (even? n) (square (fast-expt b (/ n 2)))
    :else (* b (fast-expt b (- n 1)))))

; Exercise 1.16
; Iterative exponentiation that is O(log N)

; Using a*b^n = a*(b^(n/2))^2 = a*(b^2)^(n/2)
; and a*b^n = a*b*b^(n-1) = (a*b)*(b^(n-1))

; Then 
; n even: a -> a, b -> b^2, n -> n/2
; n odd: a -> a*b, b-> b, n -> n-1

(defn fast-expt-iter [a b n]
  (cond
    (= n 0) a
    (even? n) (recur a (* b b) (/ n 2))
    :else (recur (* a b) b (- n 1))))

(defn fast-expt [b n]
  (fast-expt-iter 1 b n))

; Exercise 1.17

(defn halve [x] (/ x 2))
(defn dbl [x] (* x 2))

(defn fast-mult [a b]
  (cond
    (= b 0) 0
    (even? b) (dbl (fast-mult a (halve b)))
    :else (+ a (fast-mult a (- b 1)))))

; Exercise 1.18

(defn fast-mult-iter [a b c]
  (cond
    (= b 0) c
    (even? b) (fast-mult-iter (dbl a) (halve b) c)
    :else (fast-mult-iter a (- b 1) (+ a c))))

(defn fast-mult [a b]
  (fast-mult-iter a b 0))


; Exercise 1.19
; Tpq(a, b) = a -> (p + q)a + qb
;             b -> qa + pb
; Applying this twice gives:
; Tpq^2(a, b) = a -> ((p+q)((p+q)a + qb) + q (qa + pb)
;               b -> q((p+q)a+qb) + p(qa+pb)

; Then find p', q' such that: Tp'q' = Tpq^2

; Eq 1: (p'+q')a + q'b = ((p+q)((p+q)a + qb) + q (qa + pb)
; Eq 2: q'a + p'b = q((p+q)a+qb) + p(qa+pb)

; Then do about a page or two of algebra to get:
; p' = p^2 + q^2
; q' = q^2 + 2pq

(defn fib-iter [a b p q count]
  (cond
    (= count 0) b
    (even? count) (recur a b (+ (* p p) (* q q)) (+ (* q q) (* 2 p q)) (/ count 2))
    :else (recur (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1))))

(defn fib [n]
  (fib-iter 1 0 0 1 n))

; Exercise 1.20

(defn gcd [a b]
  (if (= b 0) a
      (recur b (mod a b))))

; normal order (fully expand before evaluating)
; except for conditionals, where the predicate is immediately evaluated
; and the result determines whether to evaluate the consequent or the alternative expression

; (gcd 206 40)
; a = 206, b = 40
; evaluate b in predicate: 0 remainders

; (gcd 40 (mod 206 40))
; a = 40, b = (mod 206 40)
; evaluate b in predicate: *1 remainder*

; (gcd (mod 206 40) (mod 40 (mod 206 40)))
; a = (mod 206 40), b = (mod 40 (mod 206 40))
; evaluate b in predicate: *2 remainders

; (gcd (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) 
; a = (mod 40 (mod 206 40)), b = (mod (mod 206 40) (mod 40 (mod 206 40)))
; evaluate b in predicate: *4 remainders*

; (gcd (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))
; a = (mod (mod 206 40) (mod 40 (mod 206 40))), b = (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))
; evaluate bin predicate: *7 remainders*
; predicate is true!
; evaluate a: *4 remainders*

; Normal order total: 18 remainders computed

; applicative order:
; (gcd 206 40)
; a = 206, b = 40
; evaluate b in predicate: 0 remainders

; (gcd 40 (mod 206 40))
; evaluate argument: *1 remainder*
; (gcd 40 6)
; evaluate predicate: 0 remainders
; (gcd 6 (mod 40 6))
; evaluate argument: *1 remainder*
; (gcd 6 4)
; evaluate predicate: 0 remainders
; (gcd 6 (mod 6 4))
; evaluate argument: *1 remainder*
; (gcd 4 2)
; evaluate predicate: 0 remainders
; (gcd 2 (mod 4 2))
; evaluate argument: *1 remainder*
; (gcd 2 0)
; evaluate predicate: 0 remainders
; 2

; Applicative order total: 4 remainders computed

; Exercise 1.21

(defn divides? [a b]
  (= (mod b a) 0))

(defn find-divisor [n test-divisor]
  (cond
    (> (* test-divisor test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (+ test-divisor 1))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(smallest-divisor 199)
; => 199

(smallest-divisor 1999)
; => 1999

(smallest-divisor 19999)
; => 7

; Exercise 1.22
(defn prime? [n]
  (and (> n 1) (= n (smallest-divisor n))))

(defn prime-search-iter [check result n]
  (cond
    (= n (count result)) result
    (prime? check) (recur (+ check 1) (conj result check) n)
    :else (recur (+ check 1) result n)))

(defn search-for-primes [start n]
  "Finds the first n primes greater than start"
  (prime-search-iter (+ start 1) [] n))

(map #(search-for-primes % 3) [1000 10000 100000 1000000])
;([1009 1013 1019]
; [10007 10009 10037]
; [100003 100019 100043]
; [1000003 1000033 1000037])

(def primes-to-time (flatten (map #(search-for-primes % 3) [1000 10000 100000 1000000])))


#_(doseq [x primes-to-time]
  (println x)
  (time (prime? x)))

;  prime  |  time (ms)
; -------------------
; 1009    |  0.019196
; 1013    |  0.009285
; 1019    |  0.009031
; 10007   |  0.025027
; 10009   |  0.022945
; 10037   |  0.023042
; 100003  |  0.072444
; 100019  |  0.070117
; 100043  |  0.070009
; 1000003 |  0.224492
; 1000033 |  0.197744
; 1000037 |  0.196534

; The data support approximately O(sqrt(N)) growth
; The fact that 1009 takes longer than 1013 is probably
; because of Clojure's memoization, which stores intermediate results
; when a function is called to speed up computation when the same
; function is called again with the same arguments. In this case,
; we are probably seeing the evaluations of mod being memoized. This same
; reason explains why 10009 took a little longer than 10037. Some of
; the evaluations of mod were memoized when checking the smaller numbers,
; but many more evaluations had to be done when reaching a new order of magnitude.

; Exercise 1.23

(defn next-x [x]
  (if (= x 2) 3 (+ x 2)))

(defn fast-find-divisor [n test-divisor]
  (cond
    (> (* test-divisor test-divisor) n) n
    (divides? test-divisor n) test-divisor
    :else (find-divisor n (next-x test-divisor))))

(defn fast-smallest-divisor [n]
  (fast-find-divisor n 2))

(defn fast-prime? [n]
  (= n (fast-smallest-divisor n)))

#_(doseq [x primes-to-time]
  (println x)
  (time (fast-prime? x)))

;  prime  |  time (ms)
; --------------------
; 1009    |  0.041022
; 1013    |  0.012308
; 1019    |  0.011344
; 10007   |  0.028118
; 10009   |  0.027167
; 10037   |  0.027292
; 100003  |  0.100743
; 100019  |  0.077622
; 100043  |  0.077412
; 1000003 |  0.234968
; 1000033 |  0.23596
; 1000037 |  0.213353


; We are saving time by skipping divisors, but we adding an extra if statement 
; that has to be evaluated every time next is called

; Exercise 1.24

(defn expmod [base exp m]
  (cond
    (= exp 0) 1
    (even? exp) (mod (square (expmod base (/ exp 2) m)) m)
    :else (mod (* base (expmod base (- exp 1) m)) m)))

; If we want to look at really big primes, we have to use a function
; that can give us bigger random numbers than the built-in rand-int
(defn rand-bigint [n]
  (bigint (bigdec (rand n))))

(defn fermat-test [n]
  (defn try-it [a]
    (= (expmod a n n) a))
  (try-it (+ 1 (rand-bigint (- n 1)))))

(defn fermat-prime? [n times]
  (cond
    (= times 0) true
    (fermat-test n) (recur n (- times 1))
    :else false))

#_(doseq [x primes-to-time]
  (println x)
  (time (fermat-prime? x 1000)))


;  prime  |  time (ms)
; --------------------
; 1009    |  6.504148
; 1013    |  6.612591
; 1019    |  6.423497
; 10007   |  8.700837
; 10009   |  7.217906
; 10037   |  7.469801
; 100003  |  9.095217
; 100019  |  9.109494
; 100043  |  8.276873
; 1000003 |  10.411793
; 1000033 |  9.499783
; 1000037 |  10.115866

; Observed scaling is better than O(log(N))
; Could be dominated by random number generation
; which is constant

; Just for fun lets search for some really big primes

(defn fermat-prime-search-iter [check result n times]
  (cond
    (= n (count result)) result
    (fermat-prime? check times) (recur (+ check 1) (conj result check) n times)
    :else (recur (+ check 1) result n times)))

(defn fermat-search-for-primes [start n times]
  "Finds the first n primes greater than start"
  (fermat-prime-search-iter (+ start 1) [] n times))

(def big-primes (fermat-search-for-primes (bigint (java.lang.Math/pow 10 100)) 3 100))

;[10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000267N
; 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000949N
; 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001243N]

; Exercise 1.25

(defn bad-exp-mod [base exp m]
  (mod (fast-expt base exp) m))

; This won't work for testing for primes because the result of (fast-expt base exp) will be too big
; and we'll get an Integer Overflow. In expmod, we take the remainder at each iteration, so we never
; have such huge numbers.

; Exercise 1.26
; Since the multiplication function takes two arguments, each of those arguments has
; to be evaluated separately when calling it. Previously, with the call to square, we only
; had to evaluate a single argument, which square copies when it calls the multiply function

; Exercise 1.27

(defn carm-iter [n a]
  (cond
    (= a n) true
    (= (expmod a n n) a) (recur n (inc a))
    :else false))

(defn is-carm? [n]
  (and  (not (prime? n)) (carm-iter n 1)))

(map is-carm? [561 1105 1729 2465 2821 6601])
; (true true true true true true)

; Find first N Carmichael numbers:

(defn first-n-carm [n]
  (take n (filter is-carm? (range))))

(first-n-carm 10)
; (561 1105 1729 2465 2821 6601 8911 10585 15841 29341)

; Exercise 1.28

(defn expmod-mr [base exp m]
  (cond
    (= exp 0) 1
    (even? exp) (let [x (expmod-mr base (/ exp 2) m)
                      x2 (mod (square x) m)]
                  (if (and (= x2 1) (not= x 1) (not= x (dec m)))
                    0
                    x2))
    :else (mod (* base (expmod-mr base (- exp 1) m)) m)))

(defn mr-iter [n a]
  (cond
    (> a (/ n 2)) true ; check first half of numbers to guarantee we find a non-trivial sqrt of 1 mod n
    (= (expmod-mr a (dec n) n) 1) (recur n (inc a))
    :else false))

(defn mr-prime? [n]
  (if (< n 2)
    false
    (mr-iter n 1)))

; Use Miller-Rabin test to find first 10 primes
(take 10 (filter mr-prime? (range)))
; (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)

; User Miller-Rabin test to check the 12 primes from previous exercises
(map mr-prime? primes-to-time)
; (true true true true true true true true true true true true)

; Check that the first 10 Carmichael numbers don't fool the Miller-Rabin test
(map mr-prime? (first-n-carm 10))
; (false false false false false false false false false false)
