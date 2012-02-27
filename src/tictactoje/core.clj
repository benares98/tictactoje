(ns tictactoje.core
  (:use (clojure set)))

(def winlist [0 0 0 0 0 0 0 0])
(def user-win (ref winlist))
(def comp-win (ref winlist))

(def initial-board [, , ,
                    , , ,
                    , , ,])

(def board (ref initial-board))

(defn reset[] (dosync (ref-set user-win winlist)
                      (ref-set comp-win winlist)
                      (ref-set board initial-board)))

(def win-positions [#{0 1 2}
                    #{3 4 5}
                    #{6 7 8}, #{0 3 6} #{1 4 7} #{2 5 8}, #{0 4 8} #{2 4 6}])

(def position-win [#{0 1 3}  #{0 4}   #{0 5 7}
                   #{1 3}  #{1 4 6 7} #{1 5}
                   #{2 3 7}  #{2 4}   #{2 5 6}])

(defn update-wins [winlist pos]
  (let [inc-found (fn [pos coll val] (if (contains? coll pos) (inc val) val))]
    (dosync (ref-set winlist (map #(inc-found pos %1 %2) win-positions @winlist)))))

(defn winning-positions [winlist rank]
  (let[ranked-positions (fn[rank positions ranking] (if (= rank ranking)(positions)))]
    (mapcat #(ranked-positions rank %1 %2) win-positions @winlist)))

(defn available-positions [coll]
  (let [index (fn [coll] map vector (iterate inc 1) coll)]
    (for [[i v] (index coll) :when (empty? v)] i)))

(defn best-play
  ([pos]
     pos)
  ([pos1 pos2]
     (if (< (count (nth position-win pos1)) (count (nth position-win pos2)))
       pos2
       pos1))
  ([pos1 pos2 & more]
     (reduce blah (blah pos1 pos2) more)))

(defn positional-play [winlist rank]
  (let [positions (intersection (available-positions board) (winning-positions winlist))]
    (best-play positions)))

(defn comp-play []
  (let [enemy-rank (apply max @user-win) comp-rank (apply max @comp-win)]
    (if (> comp-rank enemy-rank)
      (positional-play comp-win comp-rank)
      (positional-play user-win enemy-rank))))

(defn play [pos]
  (do (update-wins user-win pos)
      (update-wins comp-win comp-play)))

