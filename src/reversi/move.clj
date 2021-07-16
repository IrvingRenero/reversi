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

(defn occupied?
  [board pos connection]
  "it gives a vector with color in the left-right connections of a position "
  (vec (map #(get-in board [% :occupied-by])
            (get-in board [pos connection]))))

(defn occupied-left-right?
  "it gives a vector with color in the left-right connections of a position "
  [board pos]
  (occupied? board pos :left-right))

(defn occupied-right-left?
  "it gives a vector with color in the right-left connections of a position "
  [board pos]
  (occupied? board pos :right-left))

(defn occupied-left-up?
  "it gives a vector with color in the left-up connections of a position "
  [board pos]
  (occupied? board pos :left-up))

(defn occupied-left-down?
  "it gives a vector with color in the left-down connections of a position "
  [board pos]
  (occupied? board pos :left-down))

(defn occupied-right-up?
  "it gives a vector with color in the right-up connections of a position "
  [board pos]
  (occupied? board pos :right-up))

(defn occupied-right-down?
  "it gives a vector with color in the right-down connections of a position "
  [board pos]
  (occupied? board pos :right-down))

(defn occupied-up-down?
  "it gives a vector with color in the up-down connections of a position "
  [board pos]
  (occupied? board pos :up-down))

(defn occupied-down-up?
  "it gives a vector with color in the down-up connections of a position "
  [board pos]
  (occupied? board pos :down-up))

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

(defn changes
  "make all the changes for a connection"
  [board pos occupied connection add color]
  (let [board-after-add (add board pos)]
    (if (all-the-center? board pos color occupied)
      (reduce (fn [board pos] (add board pos))
              board-after-add
              (take (count (take-the-center board pos color occupied)) (connection pos)))
      board-after-add)))

(defn changes-black-right-left
  "make all the changes in right left connection if exist (after a black turn)"
  [board pos]
  (changes board pos occupied-right-left? reversi.board/connect-right-left add-a-black -1))

(defn changes-black-left-right
  "make all the changes in left-right connection if exist (after a black  turn)"
  [board pos]
  (changes board pos occupied-left-right? reversi.board/connect-left-right add-a-black -1))

(defn changes-black-up-down
  "make all the changes in up-down connection if exist (after a black  turn)"
  [board pos]
  (changes board pos occupied-up-down? reversi.board/connect-up-down add-a-black -1))

(defn changes-black-down-up
  "make all the changes in down-up connection if exist (after a black  turn)"
  [board pos]
  (changes board pos occupied-down-up? reversi.board/connect-down-up add-a-black -1))

(defn changes-black-left-up
  "make all the changes in left-up connection if exist (after a black  turn)"
  [board pos]
  (changes board pos occupied-left-up? reversi.board/connect-left-up add-a-black -1))

(defn changes-black-left-down
  "make all the changes in left-down connection if exist (after a black  turn)"
  [board pos]
  (changes board pos occupied-left-down? reversi.board/connect-left-down add-a-black -1))

(defn changes-black-right-down
  "make all the changes in right-down connection if exist (after a black  turn)"
  [board pos]
  (changes board pos occupied-right-down? reversi.board/connect-right-down add-a-black -1))

(defn changes-black-right-up
  "make all the changes in right-up connection if exist (after a black  turn)"
  [board pos]
  (changes board pos occupied-right-up? reversi.board/connect-right-up add-a-black -1))

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

(defn changes-white-right-left
  "make all the changes in right left connection if exist (after a white turn)"
  [board pos]
  (changes board pos occupied-right-left? reversi.board/connect-right-left add-a-white 1))

(defn changes-white-left-right
  "make all the changes in left-right connection if exist (after a white turn)"
  [board pos]
  (changes board pos occupied-left-right? reversi.board/connect-left-right add-a-white  1))

(defn changes-white-up-down
  "make all the changes in up-down connection if exist (after a white turn)"
  [board pos]
  (changes board pos occupied-up-down? reversi.board/connect-up-down add-a-white 1))

(defn changes-white-down-up
  "make all the changes in down-up connection if exist (after a white turn)"
  [board pos]
  (changes board pos occupied-down-up? reversi.board/connect-down-up add-a-white 1))

(defn changes-white-left-up
  "make all the changes in left-up connection if exist (after a white turn)"
  [board pos]
  (changes board pos occupied-left-up? reversi.board/connect-left-up add-a-white 1))

(defn changes-white-left-down
  "make all the changes in left-down connection if exist (after a white turn)"
  [board pos]
  (changes board pos occupied-left-down? reversi.board/connect-left-down add-a-white 1))

(defn changes-white-right-down
  "make all the changes in right-down connection if exist (after a white turn)"
  [board pos]
  (changes board pos occupied-right-down? reversi.board/connect-right-down add-a-white 1))

(defn changes-white-right-up
  "make all the changes in right-up connection if exist (after a white turn)"
  [board pos]
  (changes board pos occupied-right-up? reversi.board/connect-right-up add-a-white 1))

(defn complete-white-movement
  "make all the changes after add a black piece"
  [board pos]
  (if (valide-move? board pos)
    (reduce (fn [new-board changes-conection-fn] (changes-conection-fn new-board pos))
            board
            [changes-white-right-up
             changes-white-right-down
             changes-white-left-down
             changes-white-left-up
             changes-white-down-up
             changes-white-up-down
             changes-white-left-right
             changes-white-right-left])))


