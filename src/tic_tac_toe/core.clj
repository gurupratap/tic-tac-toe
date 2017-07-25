(ns tic-tac-toe.core
  (:gen-class))

(def board (apply vector (repeat 9 "_")))

(defn print-board [board]
   (let [rows (partition 3 board)]
     (loop [i 0]
       (when (< i (count rows))
       (println (nth rows i))
       (recur (inc i))))))

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
        (play-game new-board)))))

(defn -main
  "Starting point of the program"
  [& args]
  (play-game board)
)