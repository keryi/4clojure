; Problem 166 - Comparisons
(fn [less-than-op x y]
  (if (apply less-than-op (list x y))
    :lt
    (if (apply less-than-op (list y x))
      :gt
      :eq)))

; Problem 77 - Anagram Finder
(fn [words]
  (set
  	(filter #(> (count %) 1)
  		(map set
  			(vals (group-by sort words))))))

; Problem 90 Cartesian Product
(defn cartesian [s1 s2]
  (set (for [x s1 y s2] [x y])))

(cartesian #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})

; Problem 63 Group a Sequence
(defn group-seq [f s]
  (apply merge-with
   concat
    (for [e s] (hash-map (f e) [e]))))

(group-seq #(> % 5) [1 3 6 8])

; Problem 122 Read a Binary Number
(defn bin-to-dec [bin]
  (loop [bin (reverse bin) pos 1 sum 0]
    (if (empty? bin)
      sum
      (recur (rest bin) (bit-shift-left pos 1) (if (= \1 (first bin)) (+ sum pos) sum)))))

; Problem 88 Symmetric Difference
(defn sym-diff [s1 s2]
  (let [diff12 (clojure.set/difference s1 s2)
        diff21 (clojure.set/difference s2 s1)]
    (set (concat diff12 diff21))))

(sym-diff #{1 2 3 4 5 6} #{1 3 5 7})
(sym-diff #{:a :b :c} #{})
(sym-diff #{} #{4 5 6})
(sym-diff #{[1 2] [2 3]} #{[2 3] [3 4]})

; Problem 143 Dot Product
(defn dot-product [v1 v2]
  (reduce + (map #(reduce * %) (partition 2 (interleave v1 v2)))))

(dot-product [1 2] [3 4])
(dot-product [0 1 0] [1 0 0])
(dot-product [1 1 1] [1 1 1])

; Problem 135 Infix Calculator
(defn infix-calc [one op two & args]
  (loop [args args res (op one two)]
    (if (empty? args)
      res
      (recur (rest (rest args)) ((first args) res (second args))))))

(infix-calc 1 + 2)
(infix-calc 1 + 2 - 3)
(infix-calc 1 + 2 + 3 + 4 + 5)

; Problem 157 Indexing Sequences
(defn index-seq [seq]
  (loop [seq seq res [] index 0]
    (if (empty? seq)
      res
      (recur (rest seq) (conj res [(first res) index]) (inc index)))))

(index-seq [:a :b :c])
(index-seq [0 1 3])
(index-seq [[:foo] {:bar :baz}])

; Problem 97 Pascal Triangle
(defn pascal-row [m]
  (loop [i 1 c 1 row []]
    (if (= (+ m 1) i)
      row
      (recur (inc i) (quot (* (- m i) c) i) (conj row c)))))

(pascal-row 1)
(pascal-row 11)
(map pascal-row (range 1 6))

; Problem 120 Sum of Squared of Digits
(defn small-sqr-digits [coll]
  (let [sum-of-squared-digits (fn [x]
                        (loop [digits x sum 0]
                          (if (= digits 0)
                            sum
                            (recur (quot digits 10) (+ sum (* (rem digits 10) (rem digits 10)))))))]
    (count (filter true? (map (fn [x] (< x (sum-of-squared-digits x))) coll)))))

(small-sqr-digits (range 10))
(small-sqr-digits (range 30))
(small-sqr-digits (range 100))
(small-sqr-digits (range 1000))

; Problem 118 Re-implement Map
(defn remap [f coll]
  (if-not (empty? coll)
    (lazy-seq
      (cons (f (first coll)) (remap f (rest coll))))))

(remap inc [1 2 3 4 5])
(remap (fn [remap] nil) (range 10))
(->> (remap inc (range))
  (drop (dec 1000000))
  (take 2))

; Problem 128 Recognize Playing Cards
(defn recog-cards [sr]
  (let [get-suit
        (fn [s]
          (cond
           (= \D s) :diamond
           (= \S s) :spade
           (= \H s) :heart
           (= \C s) :club))
        get-rank
        (fn [r]
          (cond
           (= \T r) 8
           (= \J r) 9
           (= \Q r) 10
           (= \K r) 11
           (= \A r) 12
           :else (- (int r) (int \2))))]
    {:suit (get-suit (first sr)) :rank (get-rank (second sr))}))

(recog-cards "DQ")
(recog-cards "H5")
(recog-cards "CA")
(range 13)
(map (comp :rank recog-cards str)

; Problem 100 Least Common Multiple
(defn lcm [& args]
  (let [gcd (fn [x y]
              (if (= y 0)
                x
                (recur y (mod x y))))
        lcm (fn [a b]
              (* (quot a (gcd a b)) b))]
    (reduce lcm args)))

(lcm 1 2 3)
(lcm 5 3 7)

; Problem 95 To Tree or Not To Tree
(defn is-bintree? [s]
  (or (nil? s)
      (and (coll? s) (= (count s) 3) (every? is-bintree? (rest s)))))

(is-bintree? '(:a (:b nil nil) nil))

; Problem 66 Greatest Common Divisor
(fn [x y]
  (if (= y 0)
    x
    (recur y (mod x y))))

; Problem 81 Set Intersection
(fn [s1 s2]
  (let [s1s2 (clojure.set/difference s1 s2) s2s1 (clojure.set/difference s1 s2)]
    (if (empty? s1s2)
      s1
      (if (= s1 s1s2)
        #{}
        (clojure.set/difference s1 s1s2)))))

; Problem 62 Re-implement Iterate
(fn iter [f x]
  (cons x (lazy-seq (iter f (f x)))))

; Problem 41 Drop Every Nth Item
(fn [seq n]
  (if (= n 0)
    []
    (loop [i 0 r []]
      (if (= i (inc (count seq)))
        r
        (if (= (mod i n) 0)
          (recur (inc i) r)
          (recur (inc i) (conj r (get seq (dec i)))))))))

; Problem 32 Duplicate a Sequence
(fn [seq]
  (let [duplicator (fn [coll n]
                     (loop [c coll n n]
                       (if (= n 0)
                         c
                         (recur (conj c (first c)) (dec n)))))]
    (loop [s seq i 0 r ()]
      (if (empty? s)
        (reverse (duplicator r i))
        (if (= i 0)
          (recur (rest s) (inc i) (conj r (first s)))
          (if (= (first r) (first s))
            (recur (rest s) (inc i) (conj r (first s)))
            (recur (rest s) 1 (conj (duplicator r i) (first s)))))))))

; Problem 27 Palindrome Detector
(fn is-palindrome? [x]
  (if (string? x)
    (= x (clojure.string/join (reverse x)))
    (= x (reverse x))))

; Problem 30 Compress a Sequence
(fn compress-seq [seq]
  (loop [i 0 c []]
    (if (= i (count seq))
      c
      (if (not= (last c) (get seq i))
        (recur (inc i) (conj c (get seq i)))
        (recur (inc i) c)))))

; Problem 40 Interpose a Sequence
(fn [sep coll]
  (loop [rcoll (rest coll) result [(first coll)]]
    (if (empty? rcoll)
      result
      (recur (rest rcoll) (conj result sep (first rcoll))))))

; Problem 31 Pack a Sequence
(fn [seq]
  (loop [s (rest seq) r () aux (list (first seq))]
    (if (empty? s)
      (if (not (empty? aux))
        (reverse (conj r aux))
        (reverse r))
      (if (= (first s) (last aux))
        (recur (rest s) r (conj aux (first s)))
        (recur (rest s) (conj r aux) (list (first s)))))))

; Problem 42 Factorial Fun
(fn [n]
  (loop [n n fact 1]
    (if (= n 1)
      fact
      (recur (dec n) (* n fact)))))

; Problem 39 Interleave Two Seqs
(fn [seq1 seq2]
  (let [max (if (< (count seq1) (count seq2)) (count seq1) (count seq2))]
  (loop [i 0 r [] rs1 seq1 rs2 seq2]
    (if (= i max)
      r
      (recur (inc i) (conj r (first rs1) (first rs2)) (rest rs1) (rest rs2))))))

; Problem 49 Split a Sequence
(fn [n seq]
  (loop [s seq n n aux [] r []]
    (if (= n 0)
      (loop [s2 s aux2 []]
        (if (empty? s2)
          (conj r aux aux2)
          (recur (rest s2) (conj aux2 (first s2)))))
      (recur (rest s) (dec n) (conj aux (first s)) r))))

; Problem 26 Fibonacci Sequence
(fn [n]
  (loop [i 2 fib-seq '(1 1)]
    (if (= i n)
      (sort fib-seq)
      (recur (inc i) (conj fib-seq (+ (nth fib-seq 0) (nth fib-seq 1)))))))
