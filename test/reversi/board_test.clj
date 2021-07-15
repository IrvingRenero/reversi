(ns reversi.board-test
  (:require [clojure.test :refer :all]
            [reversi.board :as reversi.board]))


(deftest a-test
  (is (= 0 0)))

(deftest connect-left-right-test
  (is (= [61 62 63 64] (reversi.board/connect-left-right 60)))
  (is (= [2 3 4 5 6 7 8] (reversi.board/connect-left-right 1))))

(deftest connect-right-left-test
  (is (= []  (reversi.board/connect-right-left 1)))
  (is (= [22 21 20 19 18 17] (reversi.board/connect-right-left 23))))

(deftest connect-up-down-test
  (is (= [29 37 45 53 61] (reversi.board/connect-up-down 21)))
  (is (= [9 17 25 33 41 49 57] (reversi.board/connect-up-down 1))))

(deftest connect-down-up-test
  (is (= [32 24 16 8] (reversi.board/connect-down-up 40)))
  (is (= [55 47 39 31 23 15 7] (reversi.board/connect-down-up 63))))

(deftest connect-left-down-test
  (is (= [12 19 26 33] (reversi.board/connect-left-down 5)))
  (is (= [27 34 41] (reversi.board/connect-left-down 20))))

(deftest connect-right-down-test
  (is (= [10 19 28 37 46 55 64] (reversi.board/connect-right-down 1)))
  (is (= [32] (reversi.board/connect-right-down 23))))

(deftest connect-right-up-test
  (is (= [43 36 29 22 15 8] (reversi.board/connect-right-up 50)))
  (is (= [52 45 38 31 24] (reversi.board/connect-right-up 59))))

(deftest connect-left-up-test
  (is (= [55 46 37 28 19 10 1] (reversi.board/connect-left-up 64)))
  (is (= [] (reversi.board/connect-left-up 5))))

(deftest complete-board-test
  (is (= {1 {:left-down [],
             :left-right [2 3 4 5 6 7 8],
             :right-down [10 19 28 37 46 55 64],
             :right-up [],
             :left-up [],
             :up-down [9 17 25 33 41 49 57],
             :right-left [],
             :down-up [],
             :occupied-by 0},
          2 {:left-down [9],
             :left-right [3 4 5 6 7 8],
             :right-down [11 20 29 38 47 56],
             :right-up [],
             :left-up [],
             :up-down [10 18 26 34 42 50 58],
             :right-left [1],
             :down-up [],
             :occupied-by 0},
          3 {:left-down [10 17],
             :left-right [4 5 6 7 8],
             :right-down [12 21 30 39 48],
             :right-up [],
             :left-up [],
             :up-down [11 19 27 35 43 51 59],
             :right-left [2 1],
             :down-up [],
             :occupied-by 0},
          4 {:left-down [11 18 25],
             :left-right [5 6 7 8],
             :right-down [13 22 31 40],
             :right-up [],
             :left-up [],
             :up-down [12 20 28 36 44 52 60],
             :right-left [3 2 1],
             :down-up [],
             :occupied-by 0},
          5 {:left-down [12 19 26 33],
             :left-right [6 7 8],
             :right-down [14 23 32],
             :right-up [],
             :left-up [],
             :up-down [13 21 29 37 45 53 61],
             :right-left [4 3 2 1],
             :down-up [],
             :occupied-by 0}} (reversi.board/complete-board 5))))
