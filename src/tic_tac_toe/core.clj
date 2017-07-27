(ns tic-tac-toe.core
  (:gen-class))

(def board (apply vector (repeat 9 "_")))

(defn print-board [board]
   (let [rows (partition 3 board)]
     (loop [i 0]
       (when (< i (count rows))
       (println (nth rows i))
       (recur (inc i))))))

(defn is-board-full? [board]
  (not (some #{"_"} board)))

(defn mark-position [position sign board]
  (if (= (nth board position) "_")
    (assoc board position sign)
    (do
      (println "Position already filled")
      board)))

(defn compare-rows [board sign]
  (let [rows (partition 3 board)]
    (or (apply = sign (nth rows 0))
        (apply = sign (nth rows 1))
        (apply = sign (nth rows 2)))))

(defn compare-columns [board sign]
  (or (apply = sign (take-nth 3 board))
      (apply = sign (take-nth 3 (drop 1 board)))
      (apply = sign (take-nth 3 (drop 2 board)))))

(defn compare-diagonals [board sign]
  (or (apply = sign (take-nth 4 board))
      (apply = sign (take-nth 2 (drop-last 2 (drop 2 board))))))

(defn find-winner [board sign]
  (if (or (compare-rows board sign) (compare-columns board sign) (compare-diagonals board sign))
    sign))

(defn next-move [board]
  (if (= (:x (frequencies board)) (:o (frequencies board)))
         :x
         :o))
(defn play-game [board]
  (let [move (next-move board)]
    (println move)
    (print-board board)
    (println "Enter position you want to fill ranging from 0 to 8")
    (let [position (read-string (read-line)) new-board (mark-position position move board )]
      (if (find-winner new-board move)
        (do (print-board new-board) 
            (println (format "Game Over - Player %s won" move)))
        (if (is-board-full? new-board)
          (println "Game Over - Draw !!")
          (play-game new-board))))))


(defn game-tree [board move turn]
  (loop [i 0 
         available-moves (map first (filter #(= "_" (second %)) (map-indexed vector board)))]
    (println "")
    (when (< i (count available-moves))
      (print-board (mark-position (nth available-moves i) move board))
      (recur (inc i) available-moves))))

(defn get-game-tree [board move]
  (map first (filter #(= "_" (second %)) (map-indexed vector board))))

(defn get-score [board move player]
  (let [winner (find-winner board move)]
    (if (nil? winner)
      (if (is-board-full? board)
        0
        (if (= move player)
          (apply min (vals (get-next-move board (next-move board) player)))
          (apply max (vals (get-next-move board (next-move board) player)))))
      (if (= winner player)
        10
        -10))))

(defn get-next-move [board move player]
  (loop [i 0 available-moves (get-game-tree board move) scores-map {}]
    (if (< i (count available-moves))
      (let [ position (nth available-moves i)
            new-board (mark-position position move board)
            score (get-score new-board move player)
            new-scores-map (update-score-map position score scores-map new-board move player)]
        (recur (inc i) available-moves new-scores-map))
      scores-map)))

(defn update-score-map [position score score-map board move player]
  (if score
    (assoc score-map position score)))

(defn -main
  "Starting point of the program"
  [& args]
  (play-game board)
)
