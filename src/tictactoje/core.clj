(ns tictactoje.core
  (:use [clojure.set :only [intersection]]))

(def winlist [0 0 0 0 0 0 0 0])
(def x (ref winlist))
(def o (ref winlist))

(def initial-board [\_ \_ \_
                    \_ \_ \_
                    \_ \_ \_])

(def board (ref initial-board))

(defn reset[] (dosync (ref-set x winlist)
                      (ref-set o winlist)
                      (ref-set board initial-board)))

(def win-positions [#{0 1 2}
                    #{3 4 5}
                    #{6 7 8}, #{0 3 6} #{1 4 7} #{2 5 8}, #{0 4 8} #{2 4 6}])

(def weighted-positions [#{0 1 3}  #{0 4}   #{0 5 7}
                         #{1 3}  #{1 4 6 7} #{1 5}
                         #{2 3 7}  #{2 4}   #{2 5 6}])

(defn update-wins [player pos]
  (let [inc-found (fn [pos coll val] (if (contains? coll pos) (inc val) val))]
    (dosync (ref-set player (map #(inc-found pos %1 %2) win-positions @player)))))

(defn update-board [board pos player]
  (dosync (ref-set board (assoc @board pos player))))

(defn winning-positions [winlist rank]
  (let[ranked-positions (fn[rank positions ranking] (if (= rank ranking)positions))]
    (mapcat #(ranked-positions rank %1 %2) win-positions @winlist)))

(defn available-positions [coll]
  (let [index (fn [coll] (map vector (iterate inc 0) coll))]
    (for [[i v] (index coll) :when (= \_ v)] i)))

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
  (let [positions (intersection (set (available-positions @board)) (set (winning-positions player rank)))]
    (apply max-weight positions)))

(defn suggested-position [player enemy]
  (let [rank (apply max @player) enemy-rank (apply max @enemy)]
    (if (>= rank enemy-rank)
      (positional-play player rank)
      (positional-play enemy enemy-rank))))

(defn progress [player pos]
  (do (update-wins player pos)
      (update-board board pos (name `~player))))

(defn play
  ([player pos] (progress player pos)))

(defn auto-play [player enemy]
  (let[pos (suggested-position (player enemy))]
    (progress player pos)))
