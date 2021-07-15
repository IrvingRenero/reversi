(ns reversi.board)

(int (Math/ceil (/ 3 2)))

(defn row
  "the row number at the position"
  [position]
  (int (Math/ceil (/ position 8))))

(defn column
  "the column number at the position"
  [position]
  (- position (* (dec (row position)) 8)))

(defn connect-left-right
  "the right left connections"
  [position]
  (vec (range (inc position) (inc (* 8 (row position))))))

(defn connect-right-left
  "the right left connections"
  [position]
  (vec (reverse (range (inc (* 8 (dec (row position)))) position))))

(defn connect-up-down
  [position]
  (vec (range (+ position 8) 65 8)))

(defn connect-down-up
  [position]
  (vec (reverse (range (- position (* 8 (dec (row position)))) position 8))))

(defn connect-left-down
  [position]
  (vec (take (dec (column position)) (range (+ position 7) 64 7))))

(defn connect-right-down
  [position]
  (vec (take (- 8 (column position)) (range (+ position 9) 65 9))))

(defn connect-right-up
  [position]
  (vec (take (- 8 (column position)) (range (- position 7) 0 -7))))

(defn connect-left-up
  [position]
  (vec (take (dec (column position)) (range (- position 9) 0 -9))))

(defn connect-left-right-map
  "map with left-right conections"
  [board pos]
  (assoc-in board [pos :left-right] (connect-left-right pos)))

(defn connect-right-left-map
  "map with right-left conections"
  [board pos]
  (assoc-in board [pos :right-left] (connect-right-left pos)))

(defn connect-up-down-map
  "map with up-down conections"
  [board pos]
  (assoc-in board [pos :up-down] (connect-up-down pos)))

(defn connect-down-up-map
  "map with down-up conections"
  [board pos]
  (assoc-in board [pos :down-up] (connect-down-up pos)))

(defn connect-left-down-map
  "map with left-down conections"
  [board pos]
  (assoc-in board [pos :left-down] (connect-left-down pos)))

(defn connect-left-up-map
  "map with left-up conections"
  [board pos]
  (assoc-in board [pos :left-up] (connect-left-up pos)))

(defn connect-right-up-map
  "map with right-up conections"
  [board pos]
  (assoc-in board [pos :right-up] (connect-right-up pos)))

(defn connect-right-down-map
  "map with right-down conections"
  [board pos]
  (assoc-in board [pos :right-down] (connect-right-down pos)))

(defn all-conections-number
  "create all conections for a number"
  [board1 positions]
  (let [occupied-board (assoc-in board1 [positions :occupied-by] 0)]
    (reduce (fn [board all-connections]
              (all-connections board positions))
            occupied-board
            [connect-right-left-map connect-left-right-map connect-up-down-map connect-down-up-map connect-left-down-map connect-left-up-map connect-right-down-map connect-right-up-map])))

(defn complete-board
  "create a complete board"
  [positions]
  (reduce (fn [board positions]
            (all-conections-number board positions))
          {}
          (range 1 (inc positions))))
