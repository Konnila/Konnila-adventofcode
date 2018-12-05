(ns day3.core
  (:gen-class))

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s )))

(defn get-key-values 
  "get the required intvalues" 
  [line]
  (drop 1 (re-find  #"(\d+),(\d+): (\d+)x(\d+)" line)))

(defn rect-points [left-offset top-offset rect-width rect-height]
  (let [x-int (parse-int left-offset) y-int (parse-int top-offset) rwid (parse-int rect-width) rwhei (parse-int rect-height)]
    (for [x (range x-int (+ x-int rwid))
          y (range y-int (+ y-int rwhei))]
          (str x "_" y))))

; thank you Tuomo Virolainen for helping me with this merge-with fn
(defn inc-if-found [m1 m2]
  (merge-with (fn [& args]
                (if (= 2 (count args))
                  (inc (first args))
                  (first args))) m1 m2))

(defn update-claim [point]
  (assoc {} (keyword point) 1))

(defn combined-map [claims all-cl]
  (apply merge claims))

(defn blanket-reducer [data-map rect-entry]
  (let [left-offset (first rect-entry)
        top-offset (second rect-entry)
        rect-width (nth rect-entry 2)
        rect-height (nth rect-entry 3)
        rect-pts (rect-points left-offset top-offset rect-width rect-height)
        claims (map update-claim rect-pts)
        combined (combined-map claims data-map)]
      (inc-if-found data-map combined)
  ))


(defn -main
  [& args]
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [lines (line-seq rdr)
          combined-blanket
            (reduce
              blanket-reducer
              {}
              (map get-key-values lines))]
     (println (count (select-keys combined-blanket (for [[k v] combined-blanket :when (> v 1)] k)))))))
