(ns reversi.move
  (:require [reversi.board :as reversi.board]))

(defn occupied-by?
  "occupied by whom is the position"
  [board pos]
  (get-in board [pos :occupied-by]))

(defn add-a-white
  "put a white (1) piece at given position in the board"
  [board pos]
  (assoc-in board [pos :occupied-by] 1))

(defn add-a-black
  "put a black (-1) piece at given position in the board"
  [board pos]
  (assoc-in board [pos :occupied-by] -1))

(defn valide-move?
  "the only restriction for (for the moment) a valid move is a empty place"
  [board pos]
  (= (get-in board [pos :occupied-by]) 0))

(defn occupied-left-right?
  "it gives a vector with color in the left-right connections of a position "
  [board pos]
  (vec (map #(get-in board [% :occupied-by])
            (get-in board [pos :left-right]))))

(defn occupied-right-left?
  "it gives a vector with color in the right-left connections of a position "
  [board pos]
  (vec (map #(get-in board [% :occupied-by])
            (get-in board [pos :right-left]))))

(defn occupied-left-up?
  "it gives a vector with color in the left-up connections of a position "
  [board pos]
  (vec (map #(get-in board [% :occupied-by])
            (get-in board [pos :left-up]))))

(defn occupied-left-down?
  "it gives a vector with color in the left-down connections of a position "
  [board pos]
  (vec (map #(get-in board [% :occupied-by])
            (get-in board [pos :left-down]))))

(defn occupied-right-up?
  "it gives a vector with color in the right-up connections of a position "
  [board pos]
  (vec (map #(get-in board [% :occupied-by])
            (get-in board [pos :right-up]))))

(defn occupied-right-down?
  "it gives a vector with color in the right-down connections of a position "
  [board pos]
  (vec (map #(get-in board [% :occupied-by])
            (get-in board [pos :right-down]))))

(defn occupied-up-down?
  "it gives a vector with color in the up-down connections of a position "
  [board pos]
  (vec (map #(get-in board [% :occupied-by])
            (get-in board [pos :up-down]))))

(defn occupied-down-up?
  "it gives a vector with color in the down-up connections of a position "
  [board pos]
  (vec (map #(get-in board [% :occupied-by])
            (get-in board [pos :down-up]))))

(defn other-equal?
  "check if there is other with the same color in a conection"
  [board pos color connection]
  (some #(= % color) (connection board pos)))

(defn take-the-center
  "a vector with the information occupied-by between 2 pieces with the same color"
  [board pos color connection]
  (vec (take-while #(not= % color) (connection board pos))))

(defn all-the-center?
  "check if all the center between two pieces with the same color are all of the opposite color"
  [board pos color connection]
  (if (other-equal? board pos color connection)
    (= (vec (take (count (vec (take-while #(not= % color) (connection board pos)))) (repeat (* -1 color))))
       (vec (take-while #(not= % color) (connection board pos))))
    false))

(defn changes-black-right-left
  "make all the changes in right left connection if exist (after a white turn)"
  [board pos]
  (let [board-after-black (add-a-black board pos)]
    (if (all-the-center? board pos -1 occupied-right-left?)
      (reduce (fn [board pos] (add-a-black board pos))
              board-after-black
              (take (count (take-the-center board pos -1 occupied-right-left?)) (reversi.board/connect-right-left pos)))
      board-after-black)))

(defn changes-black-left-right
  "make all the changes in left-right connection if exist (after a white turn)"
  [board pos]
  (let [board-after-black (add-a-black board pos)]
    (if (all-the-center? board pos -1 occupied-left-right?)
      (reduce (fn [board pos] (add-a-black board pos))
              board-after-black
              (take (count (take-the-center board pos -1 occupied-left-right?)) (reversi.board/connect-left-right pos)))
      board-after-black)))

(defn changes-black-up-down
  "make all the changes in up-down connection if exist (after a white turn)"
  [board pos]
  (let [board-after-black (add-a-black board pos)]
    (if (all-the-center? board pos -1 occupied-up-down?)
      (reduce (fn [board pos] (add-a-black board pos))
              board-after-black
              (take (count (take-the-center board pos -1 occupied-up-down?)) (reversi.board/connect-up-down pos)))
      board-after-black)))

(defn changes-black-down-up
  "make all the changes in down-up connection if exist (after a white turn)"
  [board pos]
  (let [board-after-black (add-a-black board pos)]
    (if (all-the-center? board pos -1 occupied-down-up?)
      (reduce (fn [board pos] (add-a-black board pos))
              board-after-black
              (take (count (take-the-center board pos -1 occupied-down-up?)) (reversi.board/connect-down-up pos)))
      board-after-black)))

(defn changes-black-left-up
  "make all the changes in left-up connection if exist (after a white turn)"
  [board pos]
  (let [board-after-black (add-a-black board pos)]
    (if (all-the-center? board pos -1 occupied-left-up?)
      (reduce (fn [board pos] (add-a-black board pos))
              board-after-black
              (take (count (take-the-center board pos -1 occupied-left-up?)) (reversi.board/connect-left-up pos)))
      board-after-black)))

(defn changes-black-left-down
  "make all the changes in left-down connection if exist (after a white turn)"
  [board pos]
  (let [board-after-black (add-a-black board pos)]
    (if (all-the-center? board pos -1 occupied-left-down?)
      (reduce (fn [board pos] (add-a-black board pos))
              board-after-black
              (take (count (take-the-center board pos -1 occupied-left-down?)) (reversi.board/connect-left-down pos)))
      board-after-black)))

(defn changes-black-right-down
  "make all the changes in right-down connection if exist (after a white turn)"
  [board pos]
  (let [board-after-black (add-a-black board pos)]
    (if (all-the-center? board pos -1 occupied-right-down?)
      (reduce (fn [board pos] (add-a-black board pos))
              board-after-black
              (take (count (take-the-center board pos -1 occupied-right-down?)) (reversi.board/connect-right-down pos)))
      board-after-black)))

(defn changes-black-right-up
  "make all the changes in right-up connection if exist (after a white turn)"
  [board pos]
  (let [board-after-black (add-a-black board pos)]
    (if (all-the-center? board pos -1 occupied-right-up?)
      (reduce (fn [board pos] (add-a-black board pos))
              board-after-black
              (take (count (take-the-center board pos -1 occupied-right-up?)) (reversi.board/connect-right-up pos)))
      board-after-black)))

(defn complete-black-movement
  "make all the changes after add a black piece"
  [board pos]
  (if (valide-move? board pos)
    (reduce (fn [new-board changes-conection-fn] (changes-conection-fn new-board pos))
            board
            [changes-black-right-up
             changes-black-right-down
             changes-black-left-down
             changes-black-left-up
             changes-black-down-up
             changes-black-up-down
             changes-black-left-right
             changes-black-right-left])))
