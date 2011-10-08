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

;; Cgrand
(defn neighbours [[x y :as cell]]
  (remove #{cell} 
          (for [dx [-1 0 1] dy [-1 0 1]]
            [(+ dx x) (+ dy y)])))

(defn isBorn? [neighbor-count]
  (= neighbor-count 3))

(defn survives? [neighbor-count cel live-cells]
  (and (= neighbor-count 2) (live-cells cel)))

(defn step [live-cells]
  (let [all-neighbors-of-all-cells (mapcat neighbours live-cells)
        neighbor-count-for-all-cells (frequencies all-neighbors-of-all-cells)] 
    (set (for [[cel neighbor-count] neighbor-count-for-all-cells
             :when (or (isBorn? neighbor-count) (survives? neighbor-count cel live-cells))]
         cel))))

(def blinker-a #{[1 0] [1 1] [1 2]})
(def blinker-b #{[2 1] [1 1] [0 1]})
(fact
   (step blinker-a) => blinker-b
   (take 10 (iterate step blinker-a)) => 
      (has-prefix [blinker-a blinker-b blinker-a blinker-b blinker-a]))