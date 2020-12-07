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
  (if (nil? s) '(())
      (let [the-rest (subsets (next s))]
        (concat the-rest (map #(cons (first s) %) the-rest)))))

(subsets (list 1 2 3))
; => (nil (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; This works in the following way:
; To find all the subsets of (1 2 3), first find all the subsets of (1 2 3)
; that don't contain the number 1 (subsets (next s))
; Then create a second set of subsets which is equivalent to this first set, but with 1 added to each subset.
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

; Exercise 2.33
(defn accumulate [op initial sequence]
  (if (empty? sequence)
    initial
    (op (first sequence) (accumulate op initial (rest sequence)))))

(defn my-map [f coll]
  (accumulate #(cons (f %1) %2) '() coll))

(defn my-append [seq1 seq2]
  (accumulate #(cons %1 %2) seq2 seq1))

(defn length [coll]
  (accumulate (fn [x y] (inc y)) 0 coll))

; Exercise 2.34
(defn horner-eval [x coeffs]
  (accumulate (fn [this-coeff higher-terms]
                (+ (* higher-terms x) this-coeff))
              0
              coeffs))

; Equivalently, using reduce:
(defn horner-eval-red [x coeffs]
  (reduce (fn [higher-terms this-coeff]
            (+ (* higher-terms x) this-coeff))
          (reverse coeffs)))

; Exercise 2.35
(defn count-leaves [tree]
  (accumulate (fn [x y]
                (if (seq? x)
                  (+ (count-leaves x) y)
                  (inc y)))
              0 tree))

; Exercise 2.36
(defn accumulate-n [op init seqs]
  (if (nil? (first seqs))
    nil
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map next seqs)))))

; Exercise 2.37
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map #(dot-product %1 v) m))

(defn transpose [m]
  (accumulate-n cons '() m))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))

; Exercise 2.38
(def fold-right accumulate)

(defn fold-left [op initial sequence]
  (loop [result initial
         remaining sequence]
    (if (empty? remaining)
      result
      (recur (op result (first remaining)) (rest remaining)))))

(fold-right / 1 (list 1 2 3))
; => 3/2

(fold-left / 1 (list 1 2 3))
; => 1/6

(fold-right list '() (list 1 2 3))
; => (1 (2 (3 ())))

(fold-left list '() (list 1 2 3))
; => (((() 1 ) 2) 3)

; For fold-left and fold-right to produce the same values for any sequence, the operator
; must be associative, i.e. (op (op a b) c) = (op a (op b c))

; Exercise 2.39
(defn rev-left [sequence]
  (fold-left #(cons %2 %1) '() sequence))

(defn rev-right [sequence]
  (fold-right #(concat %2 (list %1)) '() sequence))

; Exercise 2.40
(defn prime? [n]
  (and (> n 1) (not-any? #(= 0 (mod n %)) (range 2 n))))

(defn prime-sum? [pair]
  (prime? (+ (first pair) (second pair))))

(defn make-pair-sum [pair]
  (list (first pair) (second pair) (+ (first pair) (second pair))))

; Clojure has mapcat instead of flatmap
(defn unique-pairs [n]
  (mapcat (fn [i]
            (map (fn [j] (list i j))
                 (range 1 i)))
          (range 2 (inc n))))

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; Exercise 2.41
; Unique triples < n = unique triples < n-1 + unique pairs < n
(defn unique-triples [n]
  (cond
    (< n 4) '()
    :else (concat
           (map #(cons (dec n) %1) (unique-pairs (- n 2)))
           (unique-triples (dec n)))))

(defn sum-s? [nums s]
  (= s (reduce + nums)))

(defn sum-s-triples [n s]
  (filter #(sum-s? %1 s) (unique-triples n)))

(sum-s-triples 10 11)
; => ((8 2 1) (7 3 1) (6 3 2) (6 4 1) (5 4 2))

; Exercise 2.42
(def empty-board nil)

(defn safe? [k positions]
  (let [kth-queen (first (filter #(= k (second %)) positions))
        check-sum (reduce + kth-queen)
        check-diff (reduce - kth-queen)
        in-check? #(and (not= % kth-queen)
                        (or
                         (= check-sum (reduce + %))
                         (= check-diff (reduce - %))
                         (= (first %) (first kth-queen))))]
    (empty? (filter in-check? positions))))

(defn adjoin-position [new-row k rest-of-queens]
  (cons (list new-row k) rest-of-queens))

(defn queens [board-size]
  (defn queen-cols [k]
    (if (= k 0)
      (list empty-board)
      (filter
       (fn [positions] (safe? k positions))
       (mapcat
        (fn [rest-of-queens]
          (map (fn [new-row]
                 (adjoin-position new-row k rest-of-queens))
               (range 1 (inc board-size))))
        (queen-cols (dec k))))))
  (queen-cols board-size))

; Exercise 2.43
; Louis version is slow because the recursive call is happening
; many more times. In the first version, the recursive call happens 
; once for each column. In the second version, the recursive call 
; happens board-size times for each column, because it is 
; called for every row for each new column.

; For example, for k=5, the time will be 5*(T_4) where T_4 is the number
; of recursive calls for k=4, which is 5*(T_3) = 5*5*T_2 = 5*5*5*5

; So, for the eight-queens puzzle, if the first version completes in time T, 
; Louis' slower version would complete in time T^8

(def beside #(conj [] %1 %2))
(def below #(conj []  [%1] [%2]))

; Exercise 2.44
(defn right-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (right-split painter (dec n))]
      (beside painter (below smaller smaller)))))

(defn up-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller)))))


; Exercise 2.45
(defn split [f1 f2]
  (fn [painter n]
    (if (= n 0)
      painter
      (let [smaller ((split f1 f2) painter (dec n))]
        (f1 painter (f2 smaller smaller))))))

; Exercise 2.46
(defn make-vect [xcor ycor]
  (list xcor ycor))

(defn xcor-vect [v]
  (first v))

(defn ycor-vect [v]
  (second v))

(defn add-vect [v1 v2]
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(defn scale-vect [s v]
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

; Exercise 2.47
; We'll have to do some work to translate this into Clojure.
; We'll model a cons cell as a list of length 2 and
; define car/cdr as Clojure's first/second
(def car first)
(def cdr second)

; In Scheme the expression (list origin edge1 edge2)
; is syntactic sugar for (cons origin (cons edge1 (cons edge2 nil)))
; We can mimic this data structure using:
(defn make-frame1 [origin edge1 edge2]
  (list origin (list edge1 (list edge2 nil))))

(defn origin-frame1 [frame]
  (car frame))

(defn edge1-frame1 [frame]
  (car (cdr frame)))

(defn edge2-frame1 [frame]
  (car (cdr (cdr frame))))

; The second constructor is (cons origin (cons edge1 edge2))
; which we can mimic in Clojure with:
(defn make-frame [origin edge1 edge2]
  (list origin (list edge1 edge2)))

(defn origin-frame [frame]
  (car frame))

(defn edge1-frame [frame]
  (car (cdr frame)))

(defn edge2-frame [frame]
  (cdr (cdr frame)))

; Exercise 2.48
(defn make-segment [start-x start-y end-x end-y]
  (list (make-vect start-x start-y) (make-vect end-x end-y)))

(defn start-segment [segment]
  (first segment))

(defn end-segment [segment]
  (second segment))

; Dummy function because we don't have an implementation of draw-line
(defn segments->painter [segment-list]
  segment-list)

; Exercise 2.49
(def draw-outline
  (segments->painter (list
                      (make-segment 0 0 1 0)
                      (make-segment 1 0 1 1)
                      (make-segment 1 1 0 1)
                      (make-segment 0 1 0 0))))

(def draw-x
  (segments->painter (list
                      (make-segment 0 1 1 0)
                      (make-segment 0 0 1 1))))

(def draw-diamond
  (segments->painter (list
                      (make-segment 0.0 0.5 0.5 0.0)
                      (make-segment 0.5 0.0 1.0 0.5)
                      (make-segment 1.0 0.5 0.5 1.0)
                      (make-segment 0.5 1.0 0.0 0.5))))

(def draw-wave
  (segments->painter (list
                     (make-segment 0.45 1.00 0.40 0.85) ; start left side of head
                     (make-segment 0.40 0.85 0.45 0.70)
                     (make-segment 0.45 0.70 0.35 0.70) ; left shoulder
                     (make-segment 0.35 0.70 0.25 0.60) ; start left arm
                     (make-segment 0.25 0.60 0.00 0.85)
                     (make-segment 0.00 0.70 0.25 0.40)
                     (make-segment 0.25 0.40 0.37 0.55)
                     (make-segment 0.37 0.55 0.40 0.50)
                     (make-segment 0.40 0.50 0.00 0.35) ; start left leg
                     (make-segment 0.00 0.45 0.50 0.30)
                     (make-segment 0.50 0.30 0.00 0.55) ; start right leg
                     (make-segment 0.00 0.65 0.30 0.55)
                     (make-segment 0.30 0.55 1.00 0.20) ; start right arm
                     (make-segment 1.00 0.25 0.65 0.70) 
                     (make-segment 0.65 0.70 0.55 0.70) ; right shoulder
                     (make-segment 0.55 0.70 0.60 0.85) ; start right side of head
                     (make-segment 0.60 0.85 0.55 1.00))))

; Exercise 2.50
; To see the results of our transformations, I'll define a function
; that prints the four corners of a frame as a "painter" 
(defn print-corners [frame]
  (let [origin (origin-frame frame)
        edge1 (edge1-frame frame)
        edge2 (edge2-frame frame)]
    (println (add-vect origin edge2) (add-vect origin (add-vect edge1 edge2)))
    (println origin (add-vect origin edge1))))

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter
       (make-frame new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))



(defn rotate180 [painter]
  ((comp flip-vert flip-horiz) painter))

(defn rotate90 [painter]
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(defn rotate270 [painter]
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; Exercise 2.51
(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0.0)
        paint-left (transform-painter painter1
                                      (make-vect 0.0 0.0)
                                      split-point
                                      (make-vect 0.0 1.0))
        paint-right (transform-painter painter2
                                       split-point
                                       (make-vect 1.0 0.0)
                                       (make-vect 0.5 1.0))]
    (fn [frame]
      (paint-left frame)
      (paint-right frame))))

(defn below [painter1 painter2]
  (let [split-point (make-vect 0.0 0.5)
        paint-bottom (transform-painter painter1
                                      (make-vect 0.0 0.0)
                                      (make-vect 1.0 0.0)
                                      split-point)
        paint-top (transform-painter painter2
                                       split-point
                                       (make-vect 1.0 0.5)
                                       (make-vect 0.0 1.0))]
    (fn [frame]
      (paint-bottom frame)
      (paint-top frame))))

(defn below-2 [painter1 painter2]
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
