(ns game-of-life
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])
  (:use [midje.sweet]))

(defn over-populated? [nb-neighbors]
  (> nb-neighbors 3))

(defn under-populated? [nb-neighbors]
  (< nb-neighbors 2))

(defn next-state [current-state nb-neighbors]
  (if (some true? [
                   (over-populated? nb-neighbors) 
                   (under-populated? nb-neighbors)])
    :dead-cell
    :live-cell))

(fact
;;   2. Any live cell with two or three live neighbours lives on to the next generation.
  (next-state :live-cell 2) => :live-cell
  (next-state :live-cell 2) => :live-cell)

(fact
;;   1. Any live cell with fewer than two live neighbours dies, as if caused by under-population.
  (next-state :live-cell 0) => :dead-cell
  (next-state :live-cell 1) => :dead-cell)

(fact
;;   3. Any live cell with more than three live neighbours dies, as if by overcrowding.
  (next-state :live-cell 4) => :dead-cell)

(
;;   4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
