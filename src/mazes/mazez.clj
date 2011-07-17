(ns mazes.mazez  
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet])
  (:import (java.io BufferedReader FileReader))
  (:use [clojure.contrib.seq-utils :only [positions]] ))

(defn- file-name [number]
  (if (>= number 10)
    (str "src/mazes/problems/" number "_problem.txt")
    (str "src/mazes/problems/0" number "_problem.txt"))
  )

(fact
  (file-name 1) => "src/mazes/problems/01_problem.txt"
  (file-name 23) => "src/mazes/problems/23_problem.txt")

(defn maze [number]
  (with-open [f (BufferedReader. (FileReader. (file-name number))) ]
     (doall (line-seq f))))

(fact
  (maze 1) => ["#I#" "#O#"]
  (maze 2) => ["##" "IO" "##"])

(defn- move [[Ix Iy] [Ox Oy]]
  (let [dx (- Ox Ix)
        dy (- Oy Iy)]
    (cond 
      (and (= dx 0) (= dy 1) ) "E"
      (and (= dx 1) (= dy 0) ) "S"
      (and (= dx 0) (= dy -1) ) "W"
      (and (= dx -1) (= dy 0) ) "N"
      ))
  )
; 00 01 02
; 10 11 12
(defn pos [token maze] 
  ; used to find the first position of token in maze (seq of strings)
  (let [line-contains-token? (fn [ln] (.contains ln (str token)))
        line (first (filter line-contains-token?  maze))]
    [ (first (positions line-contains-token? maze)) (first (positions #{token} line ))]))

(fact
  "position of I and O"
  (pos \I ["#I#" "#O#"]) => [0 1]
  (pos \O ["#I#" "#O#"]) => [1 1]
  )

(defn solve [maze]
  (move (pos \I maze) (pos \O maze)))

(fact 
  (solve (maze 1)) => "S"
  (solve (maze 2)) => "E"
  (solve (maze 3)) => "N"
  (solve (maze 4)) => "W"
)

(defn print-maze [n]
  (def sysout (fn [text] (.println System/out text)))
  (sysout (str "===> " "maze " n " <===="))
  (doall (map sysout (maze n)))
  (sysout "")
  )