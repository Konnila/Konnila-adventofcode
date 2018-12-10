(ns day10.core
  (:gen-class))

(defn input-values [s]
  (map read-string (drop 1 (re-find #"<(.*),(.*)>.*<(.*),(.*)>" s))))

(defn draw-current-positions [coords]
  (let [sorted (sort-by (juxt second first) (map #(take 2 %) coords))
        minx (apply min (map first coords))
        maxx (+ (apply max (map first coords)) 1)
        miny (apply min (map second coords))
        maxy (apply max (map second coords))]
    (for [x (range miny (+ maxy 1)) 
          y (range minx (+ maxx 1))]
      (cond
        (and (= y maxx) (= x maxy)) "\n\n"
        (= y maxx) "\n"
        (some #(and (= y (first %)) (= x (second %))) sorted) "#"
        :else "."
      ))))

(defn coord-reducer [accv entry]
  (conj accv [(+ (first entry) (nth entry 2)) (+ (second entry) (nth entry 3)) (nth entry 2) (nth entry 3)]))

(defn -main
  [& args]
  (let [inputs (map input-values (clojure.string/split-lines (slurp "input.txt")))
        inputs-count (count inputs)
        first-x-amount  (take 1 (drop 10369 (iterate (partial reduce coord-reducer []) (vec inputs))))]
          (println (draw-current-positions (first first-x-amount)))))
