(ns mazes.mazez  
  (:use [mazes.retriever])
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet])
  (:use [clojure.contrib.seq-utils :only [positions indexed]] ))

; 00 01 02
; 10 11 12
(def directions #{[1 0]
                  [-1 0]
                  [0 1]
                  [0 -1]})
(defn- vector-add [v delta]
  (apply vector (map + v delta)))
(defn- next-positions [current]
  (map  (partial vector-add current) directions))
(fact 
  (next-positions [2 2]) => (in-any-order  '([1 2]
                                             [3 2]
                                             [2 1]
                                             [2 3])))
(defn- vector-diff [v substractor]
  (apply vector (map - v substractor)))
(defn- direction [[lastpos pos]]
    (if (some nil? [lastpos pos]) 
      nil 
      (cond 
        (= [0 1] (vector-diff pos lastpos))  "E"
        (= [1 0] (vector-diff pos lastpos)) "S"
        (= [0 -1] (vector-diff pos lastpos)) "W"
        (= [-1 0] (vector-diff pos lastpos)) "N")))
  
(fact
  (direction  '([1 1] [0 1])) => "N")


(defn pos [token maze] 
  ; find the first position of token in maze (seq of strings)
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
  ([maze lineN� colN� indexed-maze ]
    (cond 
      (empty? maze) indexed-maze
      (empty? (first maze)) (recur (next maze) (inc lineN�) 0 indexed-maze)
      true (recur 
             (cons (next (first maze)) (next maze)) 
             lineN� 
             (inc colN�) 
             (merge indexed-maze {[lineN� colN�] (first (first maze))}) ))))

(fact
  (index-maze '("#I#" "#O#")) => (in-any-order {[0 0] \# 
                                  [0 1] \I
                                  [0 2] \#
                                  [1 0] \#
                                  [1 1] \O
                                  [1 2] \#}))

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

(defn distance [a b]
  (let [delta (vector-diff b a)]
    (reduce +  (map * delta delta))))

(fact
  (distance [1 0] [1 1]) => 1
  (distance [1 1] [0 1]) => 1
  (> (distance [5 4] [0 1]) (distance [1 1] [5 4] )) => truthy
  )

(defn sorts-by-dist [a b]
  (cond 
    (< a b) -1
    (= a b) 0
    (> a b) 1
    ))
(fact
  (sorts-by-dist 1 2) => -1
  (sorts-by-dist 3 3) => 0
  (sorts-by-dist 4 0) => 1
  )
(defn move [{:keys [maze pos lastpos history] :as solve-state}]
  (def choose first)
  ;;(println "pos " pos " lastpos " lastpos)
  (def next-pos (choose (select-possible 
                               (next-positions pos) 
                               maze)))
  (merge solve-state {:history (conj history next-pos) :lastpos pos :pos next-pos} ))

(fact
  (move {:maze (index-maze '("#I#" "#.O" "###")) :pos [0 1]}) => (contains {:pos [1 1] :lastpos [0 1]})
  (move {:maze (index-maze '("#I#" "#.O" "###")) :pos [1 1]}) => (contains {:pos [1 2] :lastpos [1 1]}))

(defn finished? [solving-state]
  (def current-symbol ((:maze solving-state) (:pos solving-state)))
  (= \O current-symbol))

(defn- _solve [start-state]
  (loop [state start-state]
    (if (finished? state)
      state
      (recur (move state)))))

(defn solve [maze]
  (let  [entrance (pos \I maze)
         start-state {:maze (index-maze maze) :pos entrance :history (vector entrance)}
         last-state (_solve start-state)]
    (apply str (map direction (partition 2 1 (:history last-state)))))
  )

(fact 
  (solve (maze 2)) => "E"
  (solve (maze 3)) => "N"
  (solve (maze 4)) => "W"
  (solve (maze 5)) => "SE"
  (solve (maze 10)) => "EEEEEEEEESEENN"
)

(defn print-maze [n]
  (def sysout (fn [text] (.println System/out text)))
  (sysout (str "===> " "maze " n " <===="))
  (doall (map sysout (maze n)))
  (sysout "")
  )