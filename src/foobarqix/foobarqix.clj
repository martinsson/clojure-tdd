(ns foobarqix.foobarqix
  (:use [clojure.test])    
  (:use [midje.sweet]))

(defn- add-suffix-for-every-occ-in-number [suffix occ number]
  (apply str (for [c (re-seq occ (str number))] suffix)))

(defn- update-text-for-occurences-of [number text occ suffix]
  [number (str text (add-suffix-for-every-occ-in-number suffix occ number))])

(defn- replace-3-by-Foo [[number text]]
  (update-text-for-occurences-of number text #"3" "Foo"))

(defn- replace-5-by-Bar [[number text]]
  (update-text-for-occurences-of number text #"5" "Bar"))

(defn- replace-7-by-Qix [[number text]]
  (update-text-for-occurences-of number text #"7" "Qix"))

(defn- update-text-for-div-by [number text div suffix]
  [number (if (zero? (rem number div))
            (str text suffix)
            text)])

(defn- divisible-by-3 [[number text]]
  (update-text-for-div-by number text 3 "Foo"))

(defn- divisible-by-5 [[number text]]
  (update-text-for-div-by number text 5 "Bar"))

(defn- divisible-by-7 [[number text]]
  (update-text-for-div-by number text 7 "Qix"))

(defn- number-or-text [[number text]]
  (if (empty? text)
    (str number)
    text))

(defn fbq [n]
  (-> [n ""] divisible-by-3 divisible-by-5 divisible-by-7 replace-3-by-Foo replace-5-by-Bar replace-7-by-Qix number-or-text))

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

