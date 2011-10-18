(ns medics
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet]))


(defn box-size [medic] 10)

(defn ciel-quot [num div]
  (inc (quot (dec num) div)))

(defn nb-pills [{:keys [quantity takes-per-day days]}]
  (* quantity takes-per-day days))

(defn boxes [{:keys [posology medic]}]
  (ciel-quot (nb-pills posology) (box-size medic)))

(defprotocol PillCalc
  (nb-pills [posology]))
(defrecord ConstantPosology [quantity takes-per-day days]
  PillCalc
  (nb-pills []))
(defrecord Prescription [medic posology])
(defrecord OrderItem [medic boxes])

(defn order [prescriptions]
  (let [order-item (fn [{medic :medic :as prescription}]
                     (OrderItem. medic (boxes prescription)))]
    (map order-item prescriptions)))

;;----

(fact
  (nb-pills {:quantity 1 :takes-per-day 1 :days 5}) => 5
  (nb-pills {:quantity 2 :takes-per-day 1 :days 5}) => 10
  (nb-pills {:quantity 2 :takes-per-day 3 :days 5}) => 30
  )
(let [prescription {:posology ...posology... :medic ...medic...}] 
  (fact
    "we are given the minimum amount of boxes that covers the posology"
	  (boxes prescription) => 1
	    (provided 
	      (nb-pills ...posology...) => 1
	      (box-size ...medic...) => 10)
	  (boxes prescription) => 2
	    (provided 
	      (nb-pills ...posology...) => 20
	      (box-size ...medic...) => 10)))

(fact
  "an order can contain several prescriptions for different medics"
  (order [{:medic "paracetamol" :posology ...paracetamol...} {:medic "paracyl" :posology ...paracyl...}]) 
     => [(OrderItem. "paracetamol" 2) (OrderItem. "paracyl" 3)]
    (provided 
      (box-size "paracetamol") => 15
      (nb-pills ...paracetamol...) => 20
      (box-size "paracyl") => 7
      (nb-pills ...paracyl...) => 15
      ))