(ns euler
  (:use clojure.contrib.math)
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet]))

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

(defn possible-products []
  nil)
(fact 
  (possible-products 999) => (has-prefix [998001 997002 996004 996003 995006]))
