(ns day7.core
  (:require [clojure.set :as s])
  (:gen-class))

(defn filter-by-value [v map]
  (filter #(contains? (last %) v) map))

(defn next-node [possible-new-destinations visited state-map]
  (first 
    (filter 
      #(every? (fn [x] (visited x)) (map (fn [y] (-> y first name first)) (filter-by-value % state-map)))
      possible-new-destinations)))

(defn get-order [current destinations visited state-map]
  (if (nil? current)
      ""
      (let 
        [ stuff (do (println (str "current: " current ", visited: " visited)))
          new-destinations (do 
            (apply sorted-set (s/difference (s/union (disj destinations current) (state-map (keyword (str current)))) visited)))
            next-destination (next-node new-destinations (conj visited current) state-map)]
            (str current (get-order next-destination new-destinations (conj visited current) state-map)))))
          
(defn entry-as-map [s]
  (let [kv-pair (drop 1 (re-find #"Step (\S+) must be finished before step (\S+) can begin." s))]
    (assoc {} (keyword (first kv-pair)) (set [(-> kv-pair second first)]))))

(defn -main
  [& args]
  (let [inputs (map entry-as-map (clojure.string/split-lines (slurp "input.txt")))
        statemap (reduce #(merge-with s/union %1 %2) inputs)]
    (println (get-order \O #{\O \P \V} #{\O} statemap)))) ; I just watched from the list what are the initial available states, could be done non-manually tho lol
