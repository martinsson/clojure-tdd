(ns mazes.mazez  
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet])
  (:import (java.io BufferedReader FileReader))
  (:use [clojure.contrib.seq-utils :only [positions indexed]] ))

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

(defn- vector-diff [v substractor]
  (apply vector (map - v substractor)))
(defn- direction [{:keys [lastpos pos]}]
    (if (nil? (and lastpos pos)) nil 
    (cond 
      (= [0 1] (vector-diff pos lastpos))  "E"
      (= [1 0] (vector-diff pos lastpos)) "S"
      (= [0 -1] (vector-diff pos lastpos)) "W"
      (= [-1 0] (vector-diff pos lastpos)) "N")))
  
(fact
  (direction {:lastpos [1 1] :pos [0 1]}) => "N")


(def directions #{[1 0]
                  [-1 0]
                  [0 1]
                  [0 -1]})
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

; -> maze index-maze (while not O move (chose (remove-current (filter-possible (new-possitions current-pos))) -> map-direction 

(defn- index-maze 
  ([maze] (index-maze maze  0 0 {}))
  ([maze lineN¡ colN¡ indexed-maze ]
    (cond 
      (empty? maze) indexed-maze
      (empty? (first maze)) (recur (next maze) (inc lineN¡) 0 indexed-maze)
      true (recur 
             (cons (next (first maze)) (next maze)) 
             lineN¡ 
             (inc colN¡) 
             (merge indexed-maze {[lineN¡ colN¡] (first (first maze))}) ))))

(fact
  (index-maze '("#I#" "#O#")) => (in-any-order {[0 0] \# 
                                  [0 1] \I
                                  [0 2] \#
                                  [1 0] \#
                                  [1 1] \O
                                  [1 2] \#}))
(defn- vector-add [v delta]
  (apply vector (map + v delta)))
(defn- next-positions [current]
  (map  (partial vector-add current) directions))
(fact 
  (next-positions [2 2]) => (in-any-order  '([1 2]
                                             [3 2]
                                             [2 1]
                                             [2 3])))
(defn- walkway-or-exit? [[kee sym]]
  (#{\O \.} sym))
(defn select-possible [vicinity-coord indexed-maze]
  (keys (filter
          walkway-or-exit?
          (select-keys indexed-maze vicinity-coord))))

(fact "selects pathway or exit"
  (select-possible (next-positions [0 1]) (index-maze '("#I#" "#.O" "###"))) => '([1 1])
  (select-possible (next-positions [0 1]) (index-maze '("#I#" "#.." "###"))) => '([1 1])
  (select-possible (next-positions [1 1]) (index-maze '("#I#" "#.O" "###"))) => '([1 2])
  )
(defn move [{:keys [maze pos lastpos] :as solve-state}]
  (println "pos " pos " lastpos " lastpos)
  (merge solve-state {:lastpos pos :pos (first (select-possible 
                               (next-positions pos) 
                               maze))} ))

(fact
  (move {:maze (index-maze '("#I#" "#.O" "###")) :pos [0 1]}) => (contains {:pos [1 1] :lastpos [0 1]})
  (move {:maze (index-maze '("#I#" "#.O" "###")) :pos [1 1]}) => (contains {:pos [1 2] :lastpos [1 1]}))

(defn solve [maze]
  (let  [solution (take-while #(not= (:lastpos %) (pos \O maze)) 
              (iterate move 
                       {:maze (index-maze maze) :pos (pos \I maze)}))]
    ; ok got the sequence but how to extract the moves?
    (apply str (map direction solution)))
  )

(fact 
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