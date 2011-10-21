(ns medics
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet]))


(unfinished box-size)

(defn ciel-quot [num div]
  (inc (quot (dec num) div)))

;; sort of a java interface
(defprotocol PillCalc
  (nb-pills
    "The number of pills necessary for the whole duration of the cure"
    [posology]))

(defrecord ConstantPosology [quantity takes-per-day days]
  PillCalc  ;; The data structure ConstantPosology implements the PillCalc abstraction
  (nb-pills [posology] (* quantity takes-per-day days)))
(fact
  (nb-pills (ConstantPosology. 1 1 5)) => 5
  (nb-pills (ConstantPosology. 2 3 5)) => 30)

(defrecord VariablePosology [per-day days]
  PillCalc  ;; VariablePosology implements in another way
  (nb-pills [posology] (let [pills-per-day (apply + (vals per-day))] 
                         (* pills-per-day days))))
(fact 
  (nb-pills (VariablePosology. {:morning 2} 5)) => 10
  (nb-pills (VariablePosology. {:before-breakfast 1 :before-dinner 2} 7)) => 21)

(defn boxes [[medic posology]]
  (ciel-quot (nb-pills posology) (box-size medic)))

(defrecord OrderItem [medic boxes])

(defn order [prescriptions]
  ;; lets declare a function order-item that creates an OrderItem given a prescription-entry
  (let [order-item (fn [prescription] 
                     (OrderItem. (key prescription) (boxes prescription)))]
    ;; now we can project the list of prescription entries to a list of OrderItems 
    ;; by applying order-item to each element in prescriptions
    (map order-item prescriptions)))  

;;---- Facts about boxes and order
(fact
    "we are given the minimum amount of boxes that covers the posology"
	  (let [posology [...medic... ...posology...]] 
     (boxes posology) => 1
		    (provided 
		      (nb-pills ...posology...) => 1
		      (box-size ...medic...) => 10)
		  (boxes posology) => 2
		    (provided 
		      (nb-pills ...posology...) => 20
          (box-size ...medic...) => 10)))

(fact
  "an order can contain several prescriptions for different medics"
  (order {:paracetamol ...posology-cetamol... 
          :paracyl ...posology-paracyl...}) 
     => [(OrderItem. :paracetamol 2) (OrderItem. :paracyl 3)]
    (provided 
      (box-size :paracetamol) => 15
      (nb-pills ...posology-cetamol...) => 20
      (box-size :paracyl) => 7
      (nb-pills ...posology-paracyl...) => 15
      ))