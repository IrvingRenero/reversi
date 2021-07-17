(ns reversi.core
  (:require [reversi.board :as reversi.board]
            [reversi.move :as reversi.move]
            [reversi.io :as reversi.io]))

(defn board-maker
  "a board-maker to add white or black pieces massively (only used to test)"
  [white black]
  (reduce (fn [board pos] (reversi.move/add-a-black board pos))
          (reduce (fn [board pos] (reversi.move/add-a-white board pos))
                  (reversi.board/complete-board 64)
                  white)
          black))

(def initial-board
  (board-maker [28 37] [29 36]))

(defn start-game
  []
  (println "who start the game whites or blacks? for white press 1 or -1 for black")
  (if (= (read) 1)
    (reversi.io/white-turn initial-board))
    (reversi.io/black-turn initial-board))

(defn -main
  []
  (start-game))




