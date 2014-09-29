(ns twenty-forty-eight.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.repl :as repl]
            [clojure.math.numeric-tower :as math]))

(defn new-board-vector [n]
  (vec (repeat (* n n) 0)))

(defn permute-coords-vec [n]
  (for [x (range n)
        y (range n)]
    [x y]))

(defn cell-init []
  (if (> 0.9 (rand)) 2 4))

(defn new-board-map1 [n]
  (apply conj {}
         (for [x (range n)
               y (range n)]
           [[x y] 0])))

(defn new-board-map2 [n]
  (zipmap 
   (permute-coords-vec n) 
   (repeat 0)))


;; -----------------------------------
;; vector-of-vectors board
(defn new-board [n]
  (with-meta
    (vec (repeat n (vec (repeat n 0))))
    {:n n}))

(defn this-board [board]
  (with-meta board {:n (count board)}))

(defn pad-row-r [n row]
  (first
   (vector
    (or (first (partition n n (repeat 0) row))
        (take n (repeat 0))))))

(defn print-board [board & {:keys [msg]}]
  (println)
  (doseq [row board]
    (println (apply str (map #(format "% 5d" %) row))))
  (if msg (println msg))
  (println (meta board))
  board)

(defn randomize [board]
  (if (full? board)
    board
    (let [coord (->> (permute-coords-vec (:n (meta board)))
                     (filter (fn randomize- [x] (zero? (get-in board x))))
                     rand-nth)]
      (assoc-in board coord (cell-init)))))

(defn init-board
  ([] (init-board 4))
  ([n]
     (->> (new-board n)
          (randomize)
          (randomize))))

(defn flip-ya-cw [board]
  (apply mapv vector (reverse board)))

(def flipcounts {:l 0 :d 1 :r 2 :u 3})

(defn flip-ya [n board]
  (nth (iterate flip-ya-cw board) n))

(defn full? [board]
  (not (some zero? (apply concat board))))

(defn slammable? [board]
  (some (fn slammable?- [dir]
          (not= board (slam (dir flipcounts) board)))
        [:l :d :r :u]))

(defn detect-loss
  ([old-board new-board]
     (println :detect-loss
              (not= old-board new-board)
              (not (full? new-board))
              (slammable? new-board))
     (if (or (not= old-board new-board)
             (not (full? new-board))
             (slammable? new-board))
       new-board
       (vary-meta new-board assoc :loss? true))))

(defn slam-row [row]
  (->> row
       (remove zero?)
       (partition-by identity)
       (mapcat #(partition-all 2 %))
       (map #(apply + %))))

(defn slam [nrots board]
  (let [m (meta board)]
    (with-meta
      (->> board
           (flip-ya nrots)
           (map #(slam-row %))
           (map #(pad-row-r (:n m) %))
           (flip-ya (- 4 nrots)))
      m)))

(defn points [old-freq new-freq]
  (reduce (fn points- [r x]
            (print :x x :r r)
            (if (not= (old-freq x) (new-freq x))
              (+ r
                 (* x
                    (math/abs (- (or (new-freq x) 0)
                                 (or (old-freq x) 0)))))
              r))
          0 (rest (filter
                   #(and (not (zero? %))
                         (not= (old-freq %) (new-freq %)))
                   (sort (distinct
                          (apply concat
                                 (map keys [old-freq new-freq]))))))))

(comment
(move (this-board [[8  0 4 8]
                                            [0  0 0 2]
                                            [8  0 4 2]
                                            [8 16 0 2]])
                               :d)
  )

(defn score
  "Cannot distinguish between new slammed numbers and new random
   numbers, so must be called before randomize."
  [old-board new-board]
  (let [score (or (:score (meta new-board)) 0)
        f1 (frequencies (apply concat old-board))
        f2 (frequencies (apply concat new-board))
        points (points f1 f2)
        _ (print-board old-board)
        _ (print-board new-board)
        _ (println :f1 (map #(vector % (f1 %)) (sort (keys f1))))
        _ (println :f2 (map #(vector % (f2 %)) (sort (keys f2))))
        _ (println :points points)]
    (vary-meta new-board assoc :score (+ score points))))

(defn move [board dir]
  (if (:loss? (meta board))
    (print-board board)
    (let [n (:n (meta board))
          nrots (dir flipcounts)
          new-board (slam nrots board)
          new-board (score board new-board)
          new-board (detect-loss board new-board)
          ]
      (if (:loss? (meta new-board)) 
        (print-board new-board)
        (->> new-board
             (randomize)
             (print-board))))))

