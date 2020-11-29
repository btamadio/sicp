(ns chapter2.section2)

; Exercise 2.17
(defn last-pair [items]
  (if (nil? (next items)) (first items)
      (recur (next items))))

; Exercise 2.18
(defn reverse-list [items]
  (loop [remaining items
         result ()]
    (if (nil? remaining)
      result
      (recur (next remaining) (cons (first remaining) result)))))

; Exercise 2.19
(defn no-more? [coin-values]
  (nil? coin-values))

(defn first-denomination [coin-values]
  (first coin-values))

(defn except-first-denomination [coin-values]
  (next coin-values))

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
    (cond (nil? remaining) (reverse-list output)
          (= (even? x) (even? (first remaining))) (recur (next remaining) (cons (first remaining) output))
          :else (recur (next remaining) output))))

; A much simpler version, using filter:
(defn same-parity-simple [x & others]
  (cons x (filter #(= (even? x) (even? %)) others)))

; Exercise 2.21
(defn square [x]
  (* x x))

(defn square-list [items]
  (if (nil? items) nil
      (cons (square (first items)) (square-list (next items)))))

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
  (if (nil? items) nil
      (do
        (proc (first items))
        (recur proc (next items)))))

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

; Exercise 2.25: first/next instead of car/cdr
(first (next (first  (next (next '(1 3 (5 7) 9))))))
; => 7

(first (first '((7))))
; => 7

(first (next (first (next (first (next (first (next (first (next (first (next '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
; => 7

; Exercise 2.26
(def x (list 1 2 3))
(def y (list 4 5 6))

; Clojure equivalent to Scheme's append is concat:
(concat x y)
; => (1 2 3 4 5 6)

(cons x y)
; => ((1 2 3) 4 5 6)


(list x y)
; => ((1 2 3) (4 5 6))

; Exercise 2.27
(defn deep-reverse-list [items]
  (loop [remaining items
         result ()]
    (cond
      (nil? remaining) result
      (not (seq? (first remaining))) (recur (next remaining) (cons (first remaining) result))
      :else (recur (next remaining) (cons (deep-reverse-list (first remaining)) result)))))

; Exercise 2.28
(defn fringe [tree]
  (loop [remaining tree
         result ()]
    (cond
      (empty? remaining) result
      (seq? (first remaining)) (recur (rest remaining) (concat result (fringe (first remaining))))
      :else (recur (rest remaining) (concat result (list (first remaining)))))))

; Exercise 2.29
(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

; a
(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (second mobile))

; b
(defn total-weight [structure]
  (if (seq? structure)
    (+ (total-weight (second (left-branch structure)))
       (total-weight (second (right-branch structure))))
    structure))

; c
(defn torque [branch]
  (* (first branch) (total-weight (second branch))))

(defn balanced? [mobile]
  (or
   (not (seq? mobile))
   (and
    (= (torque (left-branch mobile)) (torque (right-branch mobile)))
    (balanced? (second (left-branch mobile)))
    (balanced? (second (right-branch mobile))))))

; d
; No code needs to be changed (in Clojure implementation)

; Exercise 2.30
(defn square-tree [tree]
  (cond (nil? tree) nil
        (not (seq? tree)) (square tree)
        :else (cons (square-tree (first tree))
                    (square-tree (next tree)))))

; using map
(defn square-tree [tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))

; Exercise 2.31
(defn tree-map [f tree]
  (map (fn [sub-tree]
         (if (seq? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

; Exercise 2.32
(defn subsets [s]
  (if (nil? s) (list nil)
      (let [the-rest (subsets (next s))]
        (concat the-rest (map #(cons (first s) %) the-rest)))))

(subsets (list 1 2 3))
; => (nil (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; This works in the following way:
; To find all the subsets of (1 2 3), first find all the subsets of (1 2 3)
; that don't contain the number 1 (subsets (next s))
; Then create a second set of subsets which is equivalent to this first set, but with 1 added.
; The union of these two sets of subsets is the set of all subsets of (1 2 3)

; We can work backwards to see how this happens:
; (subsets nil) 
;   => (nil)
; (subsets '(3)) 
;   => (concat '(nil) (map #(cons 3 %) '(nil))) 
;   => (nil (3))
; (subsets '(2 3))
;   => (concat '(nil (3)) (map #(cons 2 %) '(nil (3))) 
;   => (nil (3) (2) (2 3))) 
; (subsets '(1 2 3)) 
;   => (concat '(nil (3) (2) (2 3)) (map #(cons 1 %) '(nil (3) (2) (2 3))))
;   => (nil (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
