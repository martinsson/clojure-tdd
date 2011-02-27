(ns roman-numbers.roman-numbers
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])
  (:use [midje.sweet]))

(def roman-digits { \I 1
                   \V 5
                   \X 10
                   \L 50
                   \C 100
                   \D 500
                   \M 1000})
                  
(defn convert-digits [digits]
  (map roman-digits digits))

(defn convert [roman-number]
  (reduce +
    (map negate-if-necessary 
      (build-context
        (convert-digits roman-number)))))

(defn build-context [numbers]
  (partition 2 1 [0] numbers))

(defn negate-if-necessary [[current nextt]]
  (if (< current nextt)
    (- current)
    current))

(fact 
  (negate-if-necessary [10 100]) => -10
  (negate-if-necessary [100 100]) => 100
  (negate-if-necessary [10 10]) => 10)


(fact 
  (build-context [1000 500 5]) => [[1000 500]
                                   [500 5]
                                   [5 0]])

(facts 
  (convert "I") => 1
  (convert "V") => 5
  (convert "MI") => 1001)
  
(fact 
  (convert-digits "IVXLCDM") => [1 5 10 50 100 500 1000])
(fact 
  (convert "MMXI") => 2011)
(fact 
  (convert "MCMLXIX") => 1969)



