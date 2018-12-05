(ns day1.core
  (:gen-class))

(defn freq-reducer [acc entry]
  (if (= "+" (str (first entry)))
      (+ acc (read-string (subs entry 1)))
      (- acc (read-string (subs entry 1)))))

(defn -main
  [& args]
  (with-open [rdr (clojure.java.io/reader "input.txt")]
    (let [lines (map str (line-seq rdr))]
      (println (reduce freq-reducer 0 lines)))))
