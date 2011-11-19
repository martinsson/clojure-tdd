(ns foobarqix.foobarqix
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet]))
(defn replaceAll [regex text replacement]
  (apply str (repeat (count (re-seq regex text)) replacement))
;;  (reduce #(if (= %2 \3) (str %1 replacement) replacement) "" text)
  )

(defn append-for-occurences [number text occ suffix]
  [number (str text (replaceAll occ (str number) suffix))])

(defn replace-by-Foo [[number text]]
  (append-for-occurences number text #"3" "Foo"))

(defn replace-by-Bar [[number text]]
  (append-for-occurences number text #"5" "Bar"))

(defn replace-by-Qix [[number text]]
  (append-for-occurences number text #"7" "Qix"))

(defn divisible-by-3 [[number text]]
  (append-for-div-by number text 3 "Foo"))

(defn divisible-by-5 [[number text]]
  (append-for-div-by number text 5 "Bar"))

(defn divisible-by-7 [[number text]]
  (append-for-div-by number text 7 "Qix"))

(defn append-for-div-by [number text div suffix]
  [number (if (zero? (rem number div))
            (str text suffix)
            text)])

(defn number-or-fbq [[number text]]
  (if (empty? text)
    (str number)
    text))

(defn fbq [n]
  (-> [n ""] divisible-by-3 divisible-by-5 divisible-by-7 replace-by-Foo replace-by-Bar replace-by-Qix number-or-fbq))

(fact "simply print the number when its not a special number"
   (fbq 1) => "1"
   (fbq 2) => "2"
   (fbq 4) => "4"
   )

;; Threes
(fact "print Foo when divisible by 3"
   (fbq 6) => "Foo"
   (fbq 9) => "Foo"
   (fbq 12) => "Foo"
   )
(fact "print Foo for all 3s in the number"
   (fbq 13) => "Foo"
   (fbq 383) => "FooFoo"
   )

(fact "print Foo for both reasons"
   (fbq 36) => "FooFoo"
   (fbq 363) => "FooFooFoo"
   (fbq 333) => "FooFooFooFoo"
   )

;; Fives
(fact "print Bar when divisible by 5"
   (fbq 10) => "Bar"
   (fbq 20) => "Bar"
   )
(fact "print Bar for all 5s in the number"
   (fbq 52) => "Bar"
   (fbq 5956) => "BarBar"
   )
(fact "print Bar for both reasons"
   (fbq 25) => "BarBar"
   )

;; Both Foo and Bar
(fact "Print Foo and Bars"
   (fbq 15) => "FooBarBar"
   (fbq 135) => "FooBarFooBar"
   (fbq 35535) => "FooBarFooFooBarBarBar"
   )
(fact "7s"
   (fbq 14) => "Qix"
   (fbq 7) => "QixQix"
   (fbq 21) => "FooQix"
   (fbq 735) => "FooBarQixFooBarQix"
   )

