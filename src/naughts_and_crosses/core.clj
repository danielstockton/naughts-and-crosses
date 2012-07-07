(ns naughts-and-crosses.core)

(def players (cycle ["X" "O"]))

(defn empty-board [] (vec (repeat 9 \space)))

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

(defn winner?
  "Do we have a winner?"
  [board]
  (or
    ; ROWS
    (let [row (partition 3 board)]
      (or
        (and (apply = (first row)) (every? #(not= % \space) (first row)))
        (and (apply = (second row)) (every? #(not= % \space) (second row)))
        (and (apply = (last row)) (every? #(not= % \space) (last row)))))
    ; COLUMNS
    (let [column (apply map list (partition 3 board))]
      (or
        (and (apply = (first column)) (every? #(not= % \space) (first column)))
        (and (apply = (second column)) (every? #(not= % \space) (second column)))
        (and (apply = (last column)) (every? #(not= % \space) (last column)))))
    ; DIAGONALS
    (let [diagonal (diagonal board 3)]
      (or
        (and (apply = (first diagonal)) (every? #(not= % \space) (first diagonal)))
        (and (apply = (second diagonal)) (every? #(not= % \space) (second diagonal)))))))

(defn full? [board] (not (some #{\space} board)))

(defn empty-square? [board square] (= (board square) \space))

(defn empty-squares
  "Returns a sequence of all the empty squares"
  [board]
  (take-nth 2 (flatten (filter #(= \space (second %)) (map-indexed vector board)))))

(defn turn-tree
  "Generate all possible moves"
  [board])

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

(def state (atom (empty-board)))

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
    (if (winner? @state)
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
    (do
      (Thread/sleep 3000)
      (System/exit 0))))
