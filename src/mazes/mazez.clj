(ns mazes.mazez  
  (:use [mazes.retriever])
  (:use [clojure-tdd.core] :reload)
  (:use [clojure.test])    
  (:use [midje.sweet])
  (:use [clojure.contrib.seq-utils :only [positions indexed]] ))

; 00 01 02
; 10 11 12
(def all-directions #{[1 0]
                  [-1 0]
                  [0 1]
                  [0 -1]})
(defn- vector-add [v delta]
  (apply vector (map + v delta)))
(defn- neighbors [current]
  (map  (partial vector-add current) all-directions))
(fact 
  (neighbors [2 2]) => (in-any-order  '([1 2]
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

(defn- index-maze [maze] 
  (let [width (count (first maze))
        positions (flatten (map seq maze))
        total-size (count positions)] 
    (apply conj 
           (for [position (range total-size) 
                 :let [row    (quot position width) 
                       column (mod position width)
                       sym    (nth positions position)]] 
             { [row column]  sym}))))

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
  (select-possible (neighbors [0 1]) (index-maze '("#I#" "#.O" "###"))) => '([1 1])
  (select-possible (neighbors [0 1]) (index-maze '("#I#" "#.." "###"))) => '([1 1])
  (select-possible (neighbors [1 1]) (index-maze '("#I#" "#.O" "###"))) => '([1 2])
  )

(defn use-first [col _]
  (first col))

(defn use-random [col _]
  (let [i (.nextInt (new java.util.Random) (count col) )]
    (nth col i)))


(defn move [{:keys [maze pos lastpos history] :as solve-state}]
  (def choose use-random)
  (def possibles (select-possible (neighbors pos) maze))
  
  (if (empty? possibles)
    (merge solve-state {:history history :lastpos pos :pos nil} )
    (let [next-pos (choose possibles solve-state)]
      (merge solve-state {:history (conj history next-pos) :lastpos pos :pos next-pos} ))))

(fact
  (move {:maze (index-maze '("#I#" "#.O" "###")) :pos [0 1]}) => (contains {:pos [1 1] :lastpos [0 1]})
  (move {:maze (index-maze '("#I#" "#.O" "###")) :pos [1 1]}) => (contains {:pos [1 2] :lastpos [1 1]}))

(defn finished? [{:keys [maze pos]}]
  (def current-symbol (maze pos))
  (= \O current-symbol))

(def max-steps 5000)
(defn impossible? [{:keys [history pos]}]
  (or (nil? pos) (> (count history) max-steps)))

(defn- _solve [start-state]
  (loop [state start-state]
    (if (or (finished? state) (impossible? state))
      state
      (recur (move state)))))

(defn solve [maze]
  (let  [entrance (pos \I maze)
         start-state {:maze (index-maze maze) :pos entrance :history (vector entrance)}
         last-state (_solve start-state)]
    (if (impossible? last-state) 
      "I"
      (apply str (map direction (partition 2 1 (:history last-state))))))
  )

(fact 
  (solve (maze 2)) => "E"
  (solve (maze 3)) => "N"
  (solve (maze 4)) => "W"
  (solve (maze 5)) => "SE"
  (solve (maze 8)) => "I"
  (solve (maze 10)) => "EEEEEEEEESEENN"
;    (provided (use-random col _) => (first col))
)

(defn print-maze [n]
  (def sysout (fn [text] (.println System/out text)))
  (sysout (str "===> " "maze " n " <===="))
  (doall (map sysout (maze n)))
  (sysout "")
  )
(defn print-and-solve [n]
  (print-maze n)
  (let [solution (solve (maze n))]
    (println solution)
    (println "solved maze" n "in" (count solution))))