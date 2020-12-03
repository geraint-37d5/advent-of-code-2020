#!/usr/bin/env boot

(require '[clojure.string :as str])

(defn pi [ln]
  (->> ln
       (str/split-lines)
       (map cycle)))

(defn ct [r d mp]
  (as-> mp v
    (map #(vector %1 %2) v (range))
    (filter #(= (mod (second %) d) 0) v)
    (map first v)
    (reduce
     #(vec
       [(+ (first %1) r)
        (cond (= (nth %2 (first %1)) \#) (+ (second %1) 1)
              :else (second %1))]) [0 0] v)))

(defn s0 []
  (->> "./src/day-3/input.txt"
       (slurp)
       (pi)
       (ct 3 1)
       (second)))

(defn s1 []
  (as-> "./src/day-3/input.txt" v
       (slurp v)
       (pi v)
       (* (second (ct 1 1 v))
          (second (ct 3 1 v))
          (second (ct 5 1 v))
          (second (ct 7 1 v))
          (second (ct 1 2 v)))))

(defn -main [& args]
  (println "Day 3, Part 1")
  (println (time (s0)))
  (println "Day 3, Part 2")
  (println (time (s1))))
