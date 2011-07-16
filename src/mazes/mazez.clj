(ns mazes.mazez  
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet])
  (:import (java.io BufferedReader FileReader))
  (:use [clojure.contrib.seq-utils :only [positions]] ))

(defn file-name [number]
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

(defn move [[Ix Iy] [Ox Oy]]
  (let [dx (- Ox Ix)
        dy (- Oy Iy)]
    (cond 
      (and (= dx 0) (= dy 1) ) "S"
      (and (= dx 1) (= dy 0) ) "E"
      ))
  )

(defn pos [token maze] 
  (let [line (some #(= token %)  maze)]
    (println line)
    (first (positions token line ))))

(fact
  (pos \I ["#I#" "#O#"]) => [1 0])

(defn solve [maze]
  (move (pos "I" maze) (pos "O" maze)))

(fact 
  (solve (maze 1)) => "S"
  (provided 
    (pos "I" anything) => [1 0] 
    (pos "O" anything) => [1 1])
  (solve (maze 2)) => "E"
  (provided 
    (pos "I" anything) => [0 1] 
    (pos "O" anything) => [1 1]))