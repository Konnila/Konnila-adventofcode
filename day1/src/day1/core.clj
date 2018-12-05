(ns day1.core
  (:require [clojure.set :as s])
  (:gen-class))

(require '[clojure.set :as setns])

(defn freq-reducer [acc-map entry]
  (let [acc (:acc acc-map) 
        found-freqs (:found-freqs acc-map)
        addition (+ acc (read-string (subs entry 1)))
        substraction (- acc (read-string (subs entry 1)))]
    (if (= "+" (str (first entry)))
        (if (contains? found-freqs addition)
             (reduced addition)
             (assoc acc-map :acc addition :found-freqs (s/union found-freqs (set [addition]))))
        (if (contains? found-freqs substraction)
             (reduced substraction)
             (assoc acc-map :acc substraction :found-freqs (s/union found-freqs (set [substraction])))))))

(defn -main [& args]
  (let [lines (clojure.string/split-lines (slurp "input.txt"))]
    (println (reduce freq-reducer (assoc {} :found-freqs #{} :acc 0) (cycle lines)))))
    
