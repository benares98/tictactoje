(ns tictactoje.core
  (:use [clojure.set :only [intersection]]))

(def initial-score
  "There are eight ways to win tic tac toe.  If any of the items reaches 3 (to put it another way, when that win scenario has all of its positions covered by the player) that player is the winner"
  [0 0 0 0 0 0 0 0])

(def x (ref initial-score))
(def o (ref initial-score))

(def player-map {:x x :o o})

(def initial-queue  [:x :o :x :o :x :o :x :o :x])

(def play-queue (ref initial-queue))

(def initial-board
  "There are 9 positions on the board"
  [nil nil nil
   nil nil nil
   nil nil nil])

(def board (ref initial-board))

(defn reset[] (dosync (ref-set x initial-score)
                      (ref-set o initial-score)
                      (ref-set play-queue initial-queue)
                      (ref-set board initial-board)))

(def scoring-positions
  "This is the exhaustive set of positions that if there's player covered all the positions of just one of the sets first, then that player is the winner.  Notice that the first 3 sets cover horizontal wins, the next 3 sets cover vertical wins and the last two cover diagonal wins"
  [#{0 1 2}
   #{3 4 5}
   #{6 7 8}, #{0 3 6} #{1 4 7} #{2 5 8}, #{0 4 8} #{2 4 6}])

(def weighted-positions
  "The weights of each position is based on how many ways that position can be used to win the game.  Obviously the center position has the largest weight; it has 4 different ways that position can be used to win the game."
  [#{0 1 3}  #{0 4}   #{0 5 7}
   #{1 3}  #{1 4 6 7} #{1 5}
   #{2 3 7}  #{2 4}   #{2 5 6}])

(defn update-score [player pos]
  (let [inc-found (fn [pos coll val] (if (contains? coll pos) (inc val) val))]
    (dosync (ref-set player (map #(inc-found pos %1 %2) scoring-positions @player)))))

(defn normalize-scores
  "If the players occupy the same scenarios, then that means neither of them can win with that scenario and so set their score to a low level as to not affect the rank check"
  [] (let[x-score @x o-score @o score-overlap (fn [s1 s2](map #(if (and (> %1 0) (> %2 0)) -1 %1) s1 s2))]
       (dosync (ref-set x (score-overlap x-score o-score))
               (ref-set o (score-overlap o-score x-score)))))

(defn update-board [board pos player]
  (dosync (ref-set board (assoc @board pos player))))

(defn print-board
  ([] (print-board @board))
  ([board] (for [[x y z] (partition 3 board)] (println [x y z]))))

(defn winning-positions [player rank]
  (let[ranked-positions (fn[rank positions ranking] (if (= rank ranking)positions))]
    (mapcat #(ranked-positions rank %1 %2) scoring-positions @player)))

(defn available-positions [coll]
  (let [index (fn [coll] (map vector (iterate inc 0) coll))]
    (set (for [[i v] (index coll) :when (nil? v)] i))))

(defn max-weight
  ([pos]
     pos)
  ([pos1 pos2]
     (if (< (count (nth weighted-positions pos1)) (count (nth weighted-positions pos2)))
       pos2
       pos1))
  ([pos1 pos2 & more]
     (reduce max-weight (max-weight pos1 pos2) more)))

(defn positional-play [player rank]
  (let [positions (intersection (available-positions @board) (set (winning-positions player rank)))]
    (apply max-weight positions)))

(defn suggested-position [player enemy]
  (let [rank (apply max @player) enemy-rank (apply max @enemy)]
    (if (>= rank enemy-rank)
      (positional-play player rank)
      (positional-play enemy enemy-rank))))

(defn winner? [player] (= 3 (apply max @(player player-map))))
(defn tie? [] (empty? (available-positions @board)))

(defn play [pos]
  (let [player (peek @play-queue)]
    (if (contains? (available-positions @board) pos)
      (dosync (update-score (player player-map) pos)
              (normalize-scores)
              (update-board board pos player)
              (ref-set play-queue (pop @play-queue))
              (if (winner? player)
                (do (println "player" player "is the winner.")
                    (reset))
                (if (tie?)
                  (do (println "Nobody is the winner.  Everyone loses.")
                      (reset)))))
      (println "That position is invalid.  Try again."))))

(defn other-player [player] (if (= player x) o x))

(defn comp-play []
  (let [comp ((peek @play-queue) player-map) user (other-player comp)]
    (play (suggested-position comp user))))

;;Numbers mean positions on the board
;;[0|1|2]
;;[3|4|5]
;;[6|7|8]

;;First player is always x
;;You can let the computer be the first player by typing inputing (comp-play) at the repl
;;To play your positions input play and the number signifying the position at the repl. ex.  (play 0)
;;The board resets when a player has won  and/or that the positions have been filled up.

;;(print-board) prints the board in the repl

;;(comp-play) can be used repeatedly to have the computer play itself.  It's very boring however.  This is also the way for the computer to take over for your move.