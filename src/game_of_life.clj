(ns game-of-life
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])
  (:use [midje.sweet]))

(defn over-populated? [nb-neighbors]
  (> nb-neighbors 3))

(defn under-populated? [nb-neighbors]
  (< nb-neighbors 2))

(defn dies? [nb-neighbors]
  (if (or (under-populated? nb-neighbors) (over-populated? nb-neighbors))
    :dead-cell
    :live-cell))

(defn is-born? [nb-neighbors]
  (if (= nb-neighbors 3)
      :live-cell
      :dead-cell))

(defn next-state [current-state nb-neighbors]
  (if (= current-state :live-cell)
    (dies? nb-neighbors)
    (is-born? nb-neighbors)))


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

(fact
;;   4. Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
  (next-state :dead-cell 3) => :live-cell
  (next-state :dead-cell 2) => :dead-cell
  (next-state :dead-cell 4) => :dead-cell)

