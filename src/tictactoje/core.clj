(ns tictactoje.core)

(def winlist [0 0 0 0 0 0 0 0])
(def user-win (ref winlist))
(def comp-win (ref winlist))

(def initial-board [\_ \_ \_
                    \_ \_ \_
                    \_ \_ \_])

(def board (ref initial-board))

(defn reset[] (dosync (ref-set p1-win winlist)
                      (ref-set p2-win winlist)
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


(defn positional-play [winlist rank]
  (let [positions (intersection (available-positions board) (winning-positions winlist))]
    (if (> (count positions) 1)
      ()
      (first positions))))

(defn comp-play []
  (let [enemy-rank (apply max @enemywin) comp-rank (apply max @yourwin)]
    (if (> comp-rank enemy-rank)
      (positional-play yourwin comp-rank)
      (positional-play enemywin enemy-rank))))

(defn play [pos]
  (do (update-wins user-win pos)
      (update-wins comp-win comp-play)
      (update-screen)))

