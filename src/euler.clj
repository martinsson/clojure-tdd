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
  (= 0 (mod number div)))

(defn lazy-primes []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                (dissoc candidate)
                (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate
                (lazy-seq (next-primes (next-sieve sieve candidate)
                            (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

(defn problem3 [number primes]
  (let [current-prime (first primes)] 
    (println number current-prime)
    (cond
      (>= (* current-prime current-prime) number ) 
        number
      (divisable-by? number current-prime) 
        (recur (/ number current-prime)  primes)
      true 
        (recur number (next primes)))))

(fact
  (problem3 2 [2 3]) => 2)
(fact  
  (problem3 14 [2 3 5 7]) => 7)
(fact 
  (problem3 37 (lazy-primes)) => 37)

