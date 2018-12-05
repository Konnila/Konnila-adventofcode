(ns day5.core
  (:gen-class))

(defn reaction? [a b]
  (cond
    (Character/isUpperCase a) 
      (and 
        (Character/isLowerCase b) 
        (= (Character/toLowerCase a) b)) ; if a is uppercase, require b to be lowercase and a == b (ignored case)
    (Character/isLowerCase a)
          (and
            (Character/isUpperCase b) ; if a is lowercase, require b to be uppercase and a == b (ignored case)
            (= (Character/toUpperCase a) b))))

(defn final-enzymes [input]
  (loop [inp input
         curr-head 0
         next-head 1]
    (let [current (get inp curr-head)
          next (get inp next-head)]
      (do
        (cond
          (nil? next) inp ; end condition
          (reaction? current next) ; reaction
            (recur
              (clojure.string/join [(subs inp 0 curr-head) (subs inp (+ next-head 1) (count inp))])
              (if 
                (> curr-head 0)
                (dec curr-head)
                0)
              (if 
                (> next-head 1)
                (dec next-head)
                1))
          :else (recur inp (inc curr-head) (inc next-head)) ; no reaction
        )))))

(defn -main [& args]
  (let [input (slurp "input.txt")]
    (println (count (final-enzymes input)))))
