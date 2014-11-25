(ns ^{:todo "Refactor for beauty"}
  twenty-forty-eight.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.repl :as repl]
            [clojure.math.numeric-tower :as math]
            [incanter.stats :as stats]))

;; -----------------------------------
;; fns to make different kinds of new boards
;;(defn new-board-vector [n]
;;  (vec (repeat (* n n) 0)))
;;
;;(defn new-board-map1 [n]
;;  (apply conj {}
;;         (for [x (range n)
;;               y (range n)]
;;           [[x y] 0])))
;;
;;(defn new-board-map2 [n]
;;  (zipmap 
;;   (permute-coords-vec n) 
;;   (repeat 0)))


;; -----------------------------------
;; vector-of-vectors board
(defn new-board [n]
  (with-meta
    (vec (repeat n (vec (repeat n 0))))
    {:n n}))

(defn this-board
  "Take a specified board and add the :n meta.
   This is pretty useless and should probably be ditched soon."
  [board]
  (with-meta board {:n (count board)}))

(defn permute-coords-vec
  "Get a permutation seq of all the coordinates for a square board of
   size n."
  [n]
  (for [x (range n)
        y (range n)]
    [x y]))

(defn cell-init
  "Return a 2 90% of the time (exclusive), and a 4 the other 10%."
  []
  (if (> 0.9 (rand)) 2 4))

(defn pad-row-r
  "Pad the given row to the right with zeros up to length n.
   Returns the padded row."
  [n row]
  (first (vector (take n (concat row (repeat 0))))))

(defn print-board
  "Print the board, the meta for the board (including score and size),
   and an optional :msg.
   Returns board."
  [board & {:keys [msg]}]
  (println)
  (doseq [row board]
    (println (apply str (map #(format "% 5d" %) row))))
  (println (meta board))
  (if msg (println msg))
  board)

(defn full?
  "If board has any zeros, returns true. Else false."
  [board]
  (not (some zero? (apply concat board))))

(defn randomize
  "If the board is not full, randomly choose a cell with a 0 and
  initialize that cell.
  Returns board."
  [board]
  (if (full? board)
    board
    (let [coord (->> (permute-coords-vec (:n (meta board)))
                     (filter (fn randomize- [x] (zero? (get-in board x))))
                     rand-nth)]
      (assoc-in board coord (cell-init)))))

(defn init-board
  "Create a new board of size n (default 4), with two randomized cells."
  ([] (init-board 4))
  ([n]
     (->> (new-board n)
          (randomize)
          (randomize))))

(defn flip-ya-cw
  "Rotate the board clockwise by 90 degrees."
  [board]
  (apply mapv vector (reverse board)))

;; How many 90 degree clockwise rotations must we do to align the
;; board as if every move were a left-hand move?
(def flipcounts {:l 0 :d 1 :r 2 :u 3})

(defn flip-ya
  "Rotate the board 90 degrees clockwise, n times."
  [board n]
  (nth (iterate flip-ya-cw board) n))

(defn slam-row
  "Move the given row to the left."
  [row]
  (->> row
       (remove zero?)
       (partition-by identity)
       (mapcat #(partition-all 2 %))
       (map #(apply + %))))

(defn slam
  "Move the given board in the specified direction."
  [board dir]
  (let [m (meta board)
        nrots (flipcounts dir)]
    (with-meta
      (-> board
          (flip-ya nrots)
          (->> (map #(slam-row %))
               (map #(pad-row-r (:n m) %)))
          (flip-ya (- 4 nrots)))
      (assoc m :moves (conj (or (:moves m) []) dir)))))

(defn slammable?
  "If movement in any direction changes the board, returns true. Else false."
  [board]
  (some (fn slammable?- [dir]
          (not= board (slam board dir)))
        [:l :d]))

(defn detect-loss
  "If no move just happened, the board is full, and no movement is
   possible, return new-board with :loss? true in meta. Otherwise
   return new-board."
  [old-board new-board]
  #_
  (println :detect-loss-nope
           (not= old-board new-board)
           (not (full? new-board))
           (slammable? new-board))
  (if (or (not= old-board new-board)
          (not (full? new-board))
          (slammable? new-board))
    new-board
    (vary-meta new-board assoc :loss? true)))

(defn score
  "Return board with the :score meta updated for any changes."
  [board]
  (vary-meta board assoc :score {:max-cell (apply max (apply concat board))}))

(defn move
  "Move the given board in the specified direction."
  [board dir]
  (if (:loss? (meta board))
    board
    (let [n (:n (meta board))
          new-board (slam board dir)
          new-board (score new-board)
          new-board (detect-loss board new-board)]
      (if (:loss? (meta new-board)) 
        new-board
        (randomize new-board)))))

(defn moves
  "Move according to a given seq of moves until we run out of moves,
  or the game is lost."
  ([moves] (moves (init-board) moves))
  ([board moves]
     (if (or (empty? moves) (:loss? (meta board)))
       board
       (recur (move board (first moves)) (rest moves)))))

(defn a-not-i
  "This is a null model for validating any AIs."
  [board]
  (some (fn a-not-i- [dir]
          (when (not= board (slam board dir)) dir))
        (shuffle [:l :r :u :d])))

(defn ai-pref-dir
  "If you can move in a prioritized direction, do so."
  ([board] (ai-pref-dir board [:l :d :u :r]))
  ([board priorities]
     (some (fn ai-pref-dir- [dir]
             (when (not= board (slam board dir)) dir))
           priorities)))

(defn count-blanks [board]
  (or (get
       (frequencies (apply concat board))
       0)
      0))

(defn ai-pref-dir-watch-blanks
  "If you can move in a prioritized direction, do so, unless a threshold # of blanks has been crossed.
  If the threshold of blanks has been crossed, skip the first two
  prioritized directions and take the 3rd."
  ([board] (ai-pref-dir-watch-blanks board 1))
  ([board threshold]
     (ai-pref-dir-watch-blanks
      board
      threshold
      (array-map :l :l
                 :d :d
                 :u [:u :l :d]    ; we always want to move :l and :d immediately after :u
                 :r [:r :l :d]))) ; we always want to move :l and :d immediately after :r
  ([board threshold priorities]
     (some (fn ai-pref-dir-watch-blanks- [[dir nexts]]
             (when (not= board (slam board dir)) nexts))
           (if (<= (count-blanks board) threshold)
             (drop 2 priorities) ; fix this if priorities become pairs!
             priorities))))

(defn play-ai
  "Play any AI that is provided as a fn taking a board and returning a
  single move."
  ([ai-fn] (play-ai (init-board) ai-fn))
  ([board ai-fn]
     (if-let [d (ai-fn board)]
       (recur (moves board (if (coll? d) d [d])) ai-fn)
       board)))

(defn play-ai-dir-freqs
  "Play any provided AI and print some simple stats about movement direction frequencies."
  [& args]
  (sort (fn [[k v] [k2 v2]] (< v v2)) (-> (apply play-ai args) meta :moves frequencies)))

(defn stats-summary [data]
  {:min (apply min data)
   :max (apply max data)
   :median (stats/median data)
   :mean (stats/mean data)
   :sd (stats/sd data)
   :freq (sort-by first (frequencies data))})

(defn max-cell [board]
  (-> board meta :score :max-cell))

(defn play-ai->stats
  "Play the provided player n times and show stats (min, max, median,
  mean, sd) of max-cell scores."
  ([ai-fn] (play-ai->stats ai-fn 1000))
  ([ai-fn n] (play-ai->stats ai-fn n []))
  ([ai-fn n losing-boards]
     (stats-summary
      (map (comp max-cell deref)
           (for [_ (range n)]
             (future (play-ai ai-fn)))))))

(comment
  "How to play:"
  (def b (atom (init-board)))
  (swap! b move :u)
  (swap! b move :d)
  (swap! b move :l)
  (swap! b move :r)
  )
