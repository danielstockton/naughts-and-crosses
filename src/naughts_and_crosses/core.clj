(ns naughts-and-crosses.core
  (:require [naughts-and-crosses.ai :as ai]))

(def players (cycle [\X \O]))

(def empty-board (vec (repeat 9 \space)))

(defn board->ascii
  "Produces an ascii board representation for printing"
  [board]
  (apply str 
    (interpose "\n-+-+-\n"
      (map #(apply str (flatten %))
        (map #(interpose "|" %) (reverse (partition 3 board)))))))

(defn display [board]
  (println (board->ascii board)))

(defn rowcol->index
  "Returns index from row and column"
  [row col]
  (+ (* 3 (dec row)) (dec col)))

(defn turn
  "Place a mark in the given square"
  [board mark square]
  (assoc board square mark))

(defn diagonal [board length]
  (list
    (#(map-indexed (fn [i el] (nth el i)) %) (partition length board))
    (#(map-indexed (fn [i el] (nth el (- (count %) i 1))) %) (partition length board))))

(def win-squares
  (let [idx (partition 3 (range 9))]
    (concat idx (apply map list idx) [[0 4 8] [2 4 6]])))

(defn win?
  "Do we have a winner? Who is it? Returns X, O or nil"
  [board]
  (some (fn [[a b c]] (if (= (board a) (board b) (board c)) (if (not= (board a) \space) (board a)))) win-squares))

(defn full? [board] (not (some #{\space} board)))

(defn empty-square? [board square] (= (board square) \space))

(defn empty-squares
  "Returns a sequence of all the empty squares"
  [board]
  (take-nth 2 (flatten (filter #(= \space (second %)) (map-indexed vector board)))))

(defn moves
  "Given a board, return all possible moves"
  [board]
  (map-indexed #(turn board (nth players %1) %2) (empty-squares board)))

; TODO make it intelligent
(defn evaluate-position
  "Scores the given board position"
  [board]
  (case (win? board)
    \X 1
    \O -1
    0))

(defn choose-best-move
  "Find the best computer move for the given board position"
  [board]
  (ai/best-move (ai/game-tree board moves) evaluate-position))

(defn read-int []
  (try (Integer/parseInt (read-line))
    (catch NumberFormatException e nil)))

(defn prompt-user
  [board mark]
  (print "Choose a square (1-9):")
  (flush)
  (dec (read-int)))

(defn start-text []
  (println
    (str
      "=================\n"
      "Naughts & Crosses\n"
      "=================\n\n"
      (board->ascii (range 1 10))
      "\n")))

(def state (atom empty-board))

(defn play
  "
  The main entry point to the game.
  
  No argument -> Human vs Human
  X or O -> Computer (as given argument) vs Human
  X and O -> Computer vs Computer
  "
  ([]
  (start-text)
  (println "Human vs Human")
  (loop [in-line players]
    (reset! state (turn @state (first in-line) (prompt-user @state (first in-line))))
    (newline)
    (display @state)
    (newline)
    (if (win? @state)
      (do
        (println "You've won!")
        (System/exit 0))
      (if (full? @state)
        (do
          (println "It's a draw!")
          (System/exit 0))))
    (recur (next in-line))))
  ([computer]
    (start-text)
    (println (str "Computer (" computer ") vs Human"))
    (do
      (System/exit 0)))
  ([computer1 computer2]
    (start-text)
    (println "Computer vs Computer")
   (loop [in-line players]
    (reset! state (choose-best-move @state))
    (newline)
    (display @state)
    (newline)
    (if (win? @state)
      (do
        (println "You've won!")
        (System/exit 0))
      (if (full? @state)
        (do
          (println "It's a draw!")
          (System/exit 0))))
    (recur (next in-line)))))
