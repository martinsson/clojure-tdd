(ns primes
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet])
  (:use [clojure.contrib.combinatorics])
  (:use [euler-common]))


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
(defn problem7 []
  (nth (lazy-primes) 10000))
(defn problem10 []
  (reduce + (take-while #(< % 2000000) (lazy-primes))))

(defn prime? [n]
  (not-any? 
    (partial divisable-by? n)
    (for [divisor (iterate inc 2) :while (<= (* divisor divisor) n ) ] divisor )))
(fact 
  (prime? 10) => falsey
  (prime? 17) => truthy
  (prime? 2) => truthy)

(defn problem41 [n] 
  (first (filter 
           prime? 
           (map 
             #(Integer. (apply str %)) 
             (permutations (range n 0 -1) )))))