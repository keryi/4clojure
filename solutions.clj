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
