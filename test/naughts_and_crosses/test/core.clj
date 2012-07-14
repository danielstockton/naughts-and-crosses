(ns naughts-and-crosses.test.core
  (:use [naughts-and-crosses.core])
  (:use [clojure.test]))

(def draw ["X" "O" "X" "O" "O" "X" "X" "X" "O"])

(deftest full?-test
  (testing "Detect if the board is full"
    (is (not (full? empty-board)))
    (is (full? (vec (repeat 9 "X"))))))

(deftest win?-test
  (testing "Detect a winner on a row"
    (is (win? (vec (repeat 9 "X"))))
    (is (win? (vec (concat (repeat 3 "X") (repeat 6 \space))))))
  (testing "Detect a winner on a diagonal"
    (is (win? [\space \space "X" \space "X" \space "X" \space \space])))
  (testing "Detect a winner on a column"
    (is (win? [\space \space "X" \space \space "X" \space \space "X"])))
  (testing "Detect a draw"
    (is (not (win? draw))))
  (testing "No winner on an empty board"
    (is (not (win? empty-board)))))

(deftest rowcol->index-test
  (testing "Convert a row and column into an index"
    (is (= (rowcol->index 1 3) 2))
    (is (= (rowcol->index 2 2) 4))
    (is (= (rowcol->index 3 1) 6))))

(deftest turn-test
  (testing "Place a mark on the board in the correct place"
    (is (= (turn empty-board "X" 3) [\space \space \space "X" \space \space \space \space \space]))
    (is (= (turn empty-board "O" 5) [\space \space \space \space \space "O" \space \space \space]))))

(deftest empty-squares-test
  (testing "Get all empty squares"
    (= (empty-squares empty-board) (range 9))))

(deftest board->ascii-test
  (testing "Print the board"
    (is (= (board->ascii empty-board) " | | \n-+-+-\n | | \n-+-+-\n | | "))))
