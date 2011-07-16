(ns primes
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet])
  (:use [clojure.contrib.combinatorics]))


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

(defn prime? [prime-generator n]
  (some 
    (partial = n) 
    (for [prime (prime-generator) :while (<= prime n ) ] prime )))
(fact 
  (prime? lazy-primes 10) => falsey
  (prime? lazy-primes 17) => truthy
  (prime? lazy-primes 2) => truthy)

(defn problem41 [n] 
  (first (filter 
           (partial prime? (memoize lazy-primes)) 
           (map 
             #(Integer. (apply str %)) 
             (permutations (range n 0 -1) )))))