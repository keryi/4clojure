; Problem 166 - Comparisons
(fn [less-than-op x y]
  (if (apply less-than-op (list x y))
    :lt
    (if (apply less-than-op (list y x))
      :gt
      :eq)))