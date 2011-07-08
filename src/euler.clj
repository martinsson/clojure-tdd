(ns euler)

(defn problem1 []
  "sum of multiples of 3 and 5 below 1000"
  (apply + (set (concat (range 3 1000 3) (range 5 1000 5)))) )

