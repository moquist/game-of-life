(ns game-of-life.core)

#_
"
1) Any live cell with fewer than two live neighbours dies,
   as if caused by under-population.
2) Any live cell with two or three live neighbours lives
   on to the next generation.
3) Any live cell with more than three live neighbours dies,
   as if by overcrowding.
4) Any dead cell with exactly three live neighbours becomes
   a live cell, as if by reproduction.
"

(defn get-neighbor-xy [x y]
  [[(dec x) (dec y)] [(dec x) y] [(dec x) (inc y)]
   [x (dec y)] [x (inc y)]
   [(inc x) (dec y)] [(inc x) y] [(inc x) (inc y)]])

(defn alive? [board x y]
  (-> board
    (nth x nil)
    (nth y nil)
    #{\#}))

(defn count-friends [board x y]
  (let [xys (get-neighbor-xy x y)
        alives (map #(apply alive? board %) xys)
        friends (->> alives (filter identity) count)]
    friends))

(defn live-free [cell friends-count]
  (cond
    (= friends-count 3) \#
    (< friends-count 2) \space
    (< 3 friends-count) \space
    (= cell \#) \#
    :or-die \space))

(defn generation [board]
  (vec
    (map-indexed
      (fn [x row]
        (apply str
         (map-indexed
          (fn [y cell]
            (->> (count-friends board x y)
                 (live-free cell)))
          row)))

      board)))

(def __ generation)

(defn primordial-soup []
  (let [n 30]
    (map (fn [_]
            (apply str
              (map
                (fn [_]
                  (if (< (rand) 0.2) "#" " "))
                (range n))))
      (range n))))


(def test-boards
  {:4clojure-1
    ["      "
     " ##   "
     " ##   "
     "   ## "
     "   ## "
     "      "]
   :snhclj-1
    ["##    "
     " ##   "
     " ##   "
     "   ## "
     "   ## "
     "      "]
   :random (primordial-soup)
   :glider
   ["                                      "
    "                         #            "
    "                       # #            "
    "             ##      ##            ## "
    "            #   #    ##            ## "
    " ##        #     #   ##               "
    " ##        #   # ##    # #            "
    "           #     #       #            "
    "            #   #                     "
    "             ##                       "
    "                                      "
    "                                      "
    "                                      "
    "                                      "
    "                                      "]})

(defn tests []
  {:test-1
    (= (__ ["      "
            " ##   "
            " ##   "
            "   ## "
            "   ## "
            "      "])
       ["      "
        " ##   "
        " #    "
        "    # "
        "   ## "
        "      "])
   :test-2
    (= (__ ["     "
            "     "
            " ### "
            "     "
            "     "])
       ["     "
        "  #  "
        "  #  "
        "  #  "
        "     "])
    :test-3
    (= (__ ["      "
            "      "
            "  ### "
            " ###  "
            "      "
            "      "])
       ["      "
        "   #  "
        " #  # "
        " #  # "
        "  #   "
        "      "])})

(defn clear-screen []
  (print (str (char 27) "[2J")))

(defn print-board [board]
  (clear-screen)
  (dorun
    (map #(println (apply str (interpose " " %))) board))
  (Thread/sleep 100))

(defn main [& [test-name]]
  (let [test-name (or test-name :4clojure-1)]
    (loop [ board (-> test-name keyword test-boards)]
      (print-board board)
      (recur (generation board)))))
