(ns euler-common)

(defn divisable-by? [number div]
  (zero? (mod number div)))
