(ns medics
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet]))

(unfinished box-size prescription)

(defn ciel-quot [quantity batch-size]
  "lowest multiple of batch-size that is greater than or equal to quantity"
  (inc (quot (dec quantity) batch-size)))

(defn no-pills [{:keys [days number]}]
  (* days number))

(fact 
  "multiplies the number of pills per day by the number of days"
  (no-pills {:days 7 :number 1}) => 7
  )
(future-fact   (no-pills {:days 7 :posology [:matin :midi] :number 1}) => 14)
(defn order [p]
  (ciel-quot (no-pills p) (box-size)))

(fact
  "for 1 pill the order is 1 box"
  (order ...prescription...) => 1
  (provided
    (box-size) => 10
    (no-pills ...prescription...) => 1)
  
  "for 11 pills the order is 2 boxes of 10"
  (order ...prescription...) => 2
  (provided
    (box-size) => 10
    (no-pills ...prescription...) => 11)

  "for 10 pills the order is 1 boxes of 10"
  (order ...prescription...) => 1
  (provided
    (box-size) => 10
    (no-pills ...prescription...) => 10))


