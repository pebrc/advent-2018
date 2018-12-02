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



(defn hamming [a b]
  (count (filter true? (map (partial reduce not=) (map vector a b)))))


(defn day-2-1 []
  (->> (io/reader "resources/2.input")
       (line-seq)
       (map seq)
       (map frequencies)
       (map  (fn [f] ((juxt (partial filter (fn [[k v]] (= v 2))) (partial filter (fn [[k v]] (= v 3))))f )))
       (map (fn [t] (map #(min (count %) 1) t)))
       (reduce (fn [[l1 r1] [l2 r2]] [(+ l1 l2) (+ r1 r2)]))
       (apply *)))

(defn day-2-2 []
  (let [input (line-seq (io/reader "resources/2.input"))]
    (->> (for [x input y input]
           [(hamming x y) x y])
         (filter #(not= (first %) 0))
         (reduce (fn [acc v] (if (> (first acc) (first v)) v acc)) [Integer/MAX_VALUE])
         ((fn [[_ a b]] (map (partial reduce (fn [x y] (if (= x y) x "")))(map vector a b))))
         (apply str))))
