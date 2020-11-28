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
    (cond (empty? remaining) (reverse-list output)
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

; Exercise 2.23
(defn for-each [proc items]
  (loop [remaining items]
    (if (empty? remaining) nil
        (do
          (proc (first remaining))
          (recur (rest remaining))))))

; Exercise 2.24
; (list 1 (list 2 (list 3 4)))

; Result printed by interpreter
; (1 (2 (3 4)))

; Box and pointer
; This answer is based on Scheme, in which lists are actually
; constructed as nested cons cells:
; (list a b) is constrcuted as: (cons a (cons b nil))

; So (list 1 (list 2 (list 3 4))
; is (cons 1 (cons (cons 2 (cons (cons 3 (cons 4 nil)) nil)) nil)
; The box-and-pointer diagram is:
;  __ __    __ __
; |. |. |->|. |//|
;  -- --    -- --
;   |         |  
;  __       __ __    __ __
; | 1|     |. |. |->|. |//|
;  --       -- --    -- --
;            |        |
;           __       __ __    __ __
;          |2 |     |. |. |->|. |//|
;           --       -- --    -- --
;                     |        |
;                    __       __
;                   |3 |     |4 |
;                    --       --

; As a tree:
;      (list 1 (list 2 (list 3 4)))
;               / \
;             1   (list 2 (list 3 4))
;                      / \
;                     2  (list 3 4)
;                           / \
;                          3  4

; Exercise 2.25

; (1 3 (5 7) 9)
; (car (cdaddr x))

; ((7))
; (caar x)

; (1 (2 (3 (4 (5 (6 7))))))
; (cadadr (cadadr (cadadr x)))

; Good thing we don't have to deal with this mess in Clojure!

; Exercise 2.26
; (append x y)
; => (1 2 3 4 5 6)
; Clojure equivalent: (concat x y)

; (cons x y)
; ((1 2 3) 4 5 6)
; Same in Clojure

; (list x y)
; ((1 2 3) (4 5 6))
; Same in Clojure

; Exercise 2.27
(defn deep-reverse-list [items]
  (loop [remaining items
         result ()]
    (cond
      (empty? remaining) result
      (seq? (first remaining)) (recur (rest remaining) (cons (deep-reverse-list (first remaining)) result))
      :else (recur (rest remaining) (cons (first remaining) result)))))

; Exercise 2.28
(defn fringe [tree]
  (loop [remaining tree
         result ()]
    (cond
      (empty? remaining) result
      (seq? (first remaining)) (recur (rest remaining) (concat result (fringe (first remaining))))
      :else (recur (rest remaining) (concat result (list (first remaining)))))))
