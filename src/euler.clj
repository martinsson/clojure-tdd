(ns euler
  (:use clojure.contrib.math)
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet])
  (:use clojure.contrib.combinatorics))

(defn problem1 []
  "sum of multiples of 3 and 5 below 1000"
  (apply + (set (concat (range 3 1000 3) (range 5 1000 5)))) )

(defn fib []
  (map first (iterate (fn [[a b]] (vector b (+ a b))) [1 1])))

(defn fib-less-than [limit]
  (take-while #(< % limit) (fib)))
(defn problem2 []
  (apply + (filter even? (fib-less-than 4000000))))

(defn divisable-by? [number div]
  (zero? (mod number div)))

(defn problem3-finder [number divisors]
  (let [current-divisor (first divisors)] 
    (cond
      (>= (* current-divisor current-divisor) number ) 
        number
      (divisable-by? number current-divisor) 
        (recur (/ number current-divisor)  divisors)
      true 
        (recur number (next divisors)))))
(defn problem3 []
  (problem3-finder 600851475143 (iterate inc 2)))

(fact
  (problem3-finder 2 [2 3]) => 2)
(fact  
  (problem3-finder 14 [2 3 5 7]) => 7)
(fact 
  (problem3-finder 37 (lazy-primes)) => 37)

(defn possible-products [ceiling]
  (let [all-ints-desc (reverse (range  1 (inc ceiling)))] 
    (map (partial apply *) (cartesian-product all-ints-desc all-ints-desc))))
(fact 
   (set (possible-products 3)) => (in-any-order [9 6 4 3 2 1]))

(defn reverse-sort [coll]
  (reverse (sort coll)))
(fact 
  (reverse-sort '(3 4 2 0 1)) => '(4 3 2 1 0))

(defn palindrome? [x]
  (let [number (seq (str x))] 
    (= number (reverse number))))
(fact
  (palindrome? 3) => truthy
  (palindrome? 12) => falsey
  (palindrome? 909) => truthy
  )

(defn problem4-slow []
  (first (filter palindrome? (reverse-sort (possible-products 1000)))))

(defn palindromes [max]
  (let [left-side (map str (reverse (range max)))]
    (map #(Integer/valueOf (str % (apply str (reverse %)))) left-side)))

(fact 
  (palindromes 100) => (has-prefix [9999 9889 9779]))

(defn three-digit-divisors-of [n]
  (for [x (range 1000 100 -1) :when (and (divisable-by? n x)  (> 1000 (/ n x)))] [x (/ n x) n] ))

(defn problem4-fast []
  (ffirst (filter not-empty (map three-digit-divisors-of (palindromes 1000)))))

; problem 5
; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
(defn smallest-number-evenly-divisable-by [numbers]
  (apply * (reduce cumulated-divisors [] numbers)))
(fact 
  (smallest-number-evenly-divisable-by [1 2 3]) => 6
  (smallest-number-evenly-divisable-by [1 2 3 4]) => 12
  (smallest-number-evenly-divisable-by (range 1 11)) "=future=>" 2510
  )

(defn div-if-dividable [n div]
  (if (divisable-by? n div)
    (/ n div)
    n))

(defn next-remainder [divisors n]
  (reduce div-if-dividable n divisors))
(defn cumulated-divisors [divisors n]
  (conj divisors (next-remainder divisors n)))

(fact 
  (cumulated-divisors [2 3 5] 24) => [2 3 5 4]
  (cumulated-divisors [2 3 5] 17) => [2 3 5 17])

