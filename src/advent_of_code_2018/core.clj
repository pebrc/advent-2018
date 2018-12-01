(ns advent-of-code-2018.core
  (:require [clojure.java.io :as io] ))

(defn day-1-1 []
  (->> (io/reader "resources/1.input")
      (line-seq)
      (map read-string)
      (reduce +)))

(defn day-1-2 []
  (let [reds (->> (io/reader "resources/1.input")
                  (line-seq)
                  (map read-string)
                  (cycle)
                  (reductions +))]
    
    (first (filter
            #(number? %) (reductions
                          (fn [acc x]
                            (if-let [dup (acc x)]
                              x
                              (assoc acc x 1))) (hash-map) reds)))))
