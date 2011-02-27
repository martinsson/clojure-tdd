(ns clojure-tdd.test.core
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])
  (:use [midje.sweet]))

(deftest it-works 
  (is (= 4 (+ 3 1))))

(def a 4)

(fact 
  a => 4)
