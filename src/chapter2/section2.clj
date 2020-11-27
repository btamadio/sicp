(ns chapter2.section2)

; Exercise 2.17

(defn last-pair [items]
  (if (empty? (rest items)) (first items)
      (recur (rest items))))

; Exercise 2.18
(defn reverse-list [items]
  (loop [remaining items
         result ()]
    (if (empty? remaining)
      result
      (recur (rest remaining) (cons (first remaining) result)))))

; Exercise 2.19

(defn no-more? [coin-values]
  (empty? coin-values))

(defn first-denomination [coin-values]
  (first coin-values))

(defn except-first-denomination [coin-values]
  (rest coin-values))

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values))))

(def us-coins (list 1 5 10 25 50))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)
; => 292

(cc 100 uk-coins)
; => 4563

(cc 100 (list 50 25 10 1 5))
; => 292

; Order doesn't matter. 
; The algorithm will count all the combinations, just in a different order

; Exercise 2.20

; Without using filter, we have to iterate, cons if parity matches,
; and then reverse the list before returning (since cons adds terms by prepending)
(defn same-parity [x & others]
  (loop [remaining others
         output (list x)]
    (cond (empty? remaining) (rev-list output)
          (= (even? x) (even? (first remaining))) (recur (rest remaining) (cons (first remaining) output))
          :else (recur (rest remaining) output))))

; A much simpler version, using filter:
(defn same-parity-simple [x & others]
  (cons x (filter #(= (even? x) (even? %)) others)))

; Exercise 2.21
(defn square [x]
  (* x x))

(defn square-list [items]
  (if (empty? items) ()
      (cons (square (first items)) (square-list (rest items)))))

(square-list (list 1 2 3 4))
; => (1 4 9 16)

(defn square-list [items]
  (map square items))

(square-list (list 1 2 3 4))
; => (1 4 9 16)

; 2.22
; It returns results in reverse order because the answer is constructed by prepending
; The second method doesn't work either because answer is initially nil,
; So the first iteration tries to do: (cons nil (square (car things)))
; So the car of answer will point to nil

; 2.23

(defn for-each [proc items]
  (loop [remaining items]
    (if (empty? remaining) nil
        (do
          (proc (first remaining))
          (recur (rest remaining))))))
