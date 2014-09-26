(ns twenty-forty-eight.core
  (:require [clojure.pprint :refer [pprint]]
            [clojure.repl :as repl]))

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

(defn pad-row-r [n row]
  (first
   (vector
    (or (first (partition n n (repeat 0) row))
        (take n (repeat 0))))))

(defn print-board [board & {:keys [msg]}]
  (doseq [row board]
    (println (apply str (map #(format "% 5d" %) row))))
  (if msg (println msg))
  board)

(defn randomize [n board]
  (let [coord (->> (permute-coords-vec n)
                   (filter (fn randomize- [x] (zero? (get-in board x))))
                   rand-nth)]
    (assoc-in board coord (cell-init))))

(defn init-board
  ([] (init-board 4))
  ([n]
     (->> (new-board n)
          (randomize n)
          (randomize n))))

(defn slam-row [row]
  (->> row
       (remove zero?)
       (partition-by identity)
       (mapcat #(partition-all 2 %))
       (map #(apply + %))))

(defn flip-ya-cw [board]
  (apply mapv vector (reverse board)))

(def flipcounts {:l 0 :d 1 :r 2 :u 3})

(defn flip-ya [n board]
  (nth (iterate flip-ya-cw board) n))

(defn detect-loss [board])

(defn move [board dir]
  (let [n (:n (meta board))
        nflips (dir flipcounts)]
    (with-meta
      (->> board
           (flip-ya nflips)
           (map #(slam-row %))
           (map #(pad-row-r n %))
           (flip-ya (- 4 nflips))
           (randomize n)
           (print-board))
      {:n n})))

