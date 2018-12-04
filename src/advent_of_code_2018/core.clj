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



(defn day-3-input []
  (->> (io/reader "resources/3.input")
                    (line-seq)
                    (map (partial re-find #"#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)"))
                    (map (fn [[_ id left top width height]] (assoc {}
                                                                   :id (Integer. id)
                                                                   :left (Integer. left)
                                                                   :top (Integer.  top)
                                                                   :width (Integer. width)
                                                                   :height (Integer. height))))))


(defn day-3-1 [input]
  (->> (reduce (fn [m {:keys [left top width height]}]
                 (->> (for [x (range left (+ left  width)) y (range top (+ top height))]
                        [x y])
                      (reduce  (fn [acc [x y]] (update-in acc [x y] #(if (nil? %) 1 (inc %))))m )))
               {}
               input)
       (map (fn [[k v]] {k (into {} (filter (fn [[k v]] (not= 1 v)) v))}) )
       (into {})
       (reduce (fn [acc [k v]] (+ acc (count v))) 0)))


(defn day-3-2 [input]
  (let [expected-sizes (into {} (map (fn [{:keys [id width height]}] [id (* width height)]) input))]
    (->> (reduce (fn [m {:keys [id left top width height]}]
                   (->> (for [x (range left (+ left  width)) y (range top (+ top height))]
                          [x y])
                        (reduce  (fn [acc [x y]] (update-in acc [x y] #(if (nil? %) [1 [id]] [(inc (first %)) (conj  (second %) id)]))) m )))
                 {}
                 input)
         (map (fn [[k v]] {k (into {} (filter (fn [[k [v ids]]] (= 1 v)) v))}) )
         (into {})
         (mapcat (fn [[k v]] (map (comp first second) (vals v))))
         (frequencies)
         (filter (fn [[k v]] (= v (get expected-sizes k)))))))

