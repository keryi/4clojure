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
