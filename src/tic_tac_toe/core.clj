(ns tic-tac-toe.core
  (:gen-class))

;; We represent the grid as a vector:
;; 
;;  0 | 1 | 2
;; ---+---+---
;;  3 | 4 | 5
;; ---+---+---
;;  6 | 7 | 8
;;
;; With this representation, the winning positions are:

(def winning-positions [[0 1 2] [3 4 5] [6 7 8]
                        [0 3 6] [1 4 7] [2 5 8]
                        [0 4 8] [2 4 6]])

(def initial-grid (vec (range 9)))

(def grid-format "
 %s | %s | %s
---+---+---
 %s | %s | %s
---+---+---
 %s | %s | %s
")

(defn print-grid
  ([grid]
     (print-grid grid false))
  ([grid want-mask?]
     (apply printf grid-format (map #(if (and want-mask? (number? %)) " " %) grid))))

(defn winning-position?
  "Given a grid and a player (\"O\" or \"X\"), return true if the grid
  represents a winning position for this player, otherwise false"
  [grid player]
  (some (fn [pos] (every? #(= player %) (map grid pos))) winning-positions))

(defn possible-moves
  "Return a list of possible moves for the given grid"
  [grid]
  (filter #(not (#{"O" "X"} (grid %))) (range 9)))

(defn make-move
  "Return a grid in which player (\"O\" or \"X\") has played at pos (0-8)"
  [grid player pos]
  (assoc grid pos player)) 

(defn opponent
  "Return the opposing player"
  [player]
  (if (= player "X") "O" "X"))

(declare evaluate-grid)

(defn best-move-for
  [grid player]
  (let [moves (possible-moves grid)
        scores (zipmap moves (map #(evaluate-grid (make-move grid player %) player 1) moves))
        best-score (reduce max (vals scores))
        best-moves (filter #(= (scores %) best-score) moves)]
    (rand-nth best-moves)))

(defn evaluate-grid
  [grid player win-score]
  (cond
   (winning-position? grid player) win-score
   (winning-position? grid (opponent player)) (* -1 win-score)
   (empty? (possible-moves grid)) 0
   :else (let [opp (opponent player)
               next-grid (make-move grid opp (best-move-for grid opp))]
           (recur next-grid opp (* -1 win-score)))))

(def evaluate-grid (memoize evaluate-grid))

;; (defn computer-computer-game-loop
;;   [grid player]
;;   (print-grid grid true)
;;   (cond
;;    (winning-position? grid player) (println player " wins")
;;    (winning-position? grid (opponent player)) (println (opponent player) " wins")
;;    (empty? (possible-moves grid)) (println "It's a draw")
;;    :else (recur (make-move grid player (best-move-for grid player))
;;                 (opponent player))))

(defn get-user-input
  [prompt]
  (print (str prompt " "))
  (flush)
  (read-line))

(defn yes-no-prompt
  [prompt]
  (loop []
    (let [response (get-user-input prompt)]
      (cond
       (re-seq #"^(y|Y|yes|Yes|YES)$" response) true
       (re-seq #"^(n|N|no|No|NO)$" response) false
       :else (recur)))))

(defn solicit-move
  [player valid-moves]
  (let [valid-move? (set (map str valid-moves))
        prompt (str "Enter move for " player "'s:")]
    (loop []
      (let [response (get-user-input prompt)]
        (if (valid-move? response) (Integer/parseInt response) (recur))))))

(defn play-game
  []
  (let [play-first? (yes-no-prompt "Play first?")
        human (if play-first? "X" "O")
        computer (opponent human)]
    (loop [grid initial-grid who-to-play (if play-first? human computer)]
      (print-grid grid)
      (cond
       (winning-position? grid human) (println "You win.")
       (winning-position? grid computer) (println "You lose.")
       (empty? (possible-moves grid)) (println "It's a draw.")
       :else (let [move (if (= who-to-play human)
                          (solicit-move human (possible-moves grid))
                          (best-move-for grid computer))]
               (recur (make-move grid who-to-play move) (opponent who-to-play)))))))

(defn -main []
  (loop []
    (play-game)
    (when (yes-no-prompt "Play again?") (recur))))
