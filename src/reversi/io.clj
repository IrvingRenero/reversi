(ns reversi.io
  (:require [reversi.board :as reversi.board]
            [reversi.move :as reversi.move]))

(declare black-turn)


(defn how-many-pieces
  "count how many pieces of a color there are in board"
  [board color]
  (count (filter #(= % color) (map #(get-in board [% :occupied-by]) (range 1 65)))))

(defn game-over
  "Announce the game is over and who is the winner"
  [board]
  (let [total-white (how-many-pieces board 1)
        total-black (how-many-pieces board -1)
        winner (if (> (how-many-pieces board 1) (reversi.io/how-many-pieces board -1))
                 "white"
                 "black")]
    (println "the black had: " total-black "the white had " total-white "; the winner is" winner )))

(defn white-turn
  [board]
  (println (reversi.board/print-board board))
  (println "choose the number where you want put a piece you will be + for white")
  (let [pos (read)]
    (if (reversi.move/valide-move? board pos)
     (let [new-board (reversi.move/complete-white-movement board pos)]
    (if (> (how-many-pieces new-board 0) 0)
    (black-turn new-board)
    (game-over new-board)))
     (do (println "ingresaste una casilla invalida, escoge otra")
         (white-turn board)))))

(defn black-turn
  [board]
  (println (reversi.board/print-board board))
  (println "choose the number where you want put a piece you will be - for black")
  (let [pos (read)]
    (if (reversi.move/valide-move? board pos)
      (let [new-board (reversi.move/complete-black-movement board pos)]
        (if (> (how-many-pieces new-board 0) 0)
          (white-turn new-board)
          (game-over new-board)))
      (do (println "ingresaste una casilla invalida, escoge otra")
          (black-turn board)))))






