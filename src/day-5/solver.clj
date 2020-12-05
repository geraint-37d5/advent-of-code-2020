#!/usr/bin/env boot

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn mid [[min max] act]
  (let [m (/ (- max min) 2)]
    (cond
      (or (= act \F)
          (= act \L)) (+ (Math/floor m) min)
      :else           (+ (Math/ceil  m) min))))

(defn part [[min max] act]
  (let [m (mid [min max] act)]
    (cond (= m 1.0)       (vector max max)
          (= m 0.0)       (vector min min)
          (or (= act \F)
              (= act \L)) (vec (sort (vector min m)))
          :else           (vec (sort (vector m max))))))

(defn id [bp]
  (let [spc (str/split bp #"(?<=[FB])(?=[LR])")
        rsp (first spc)
        csp (second spc)
        row (first (reduce part [0 127] (vec rsp)))
        col (first (reduce part [0 7] (vec csp)))]
    (int (+ (* row 8) col))))

(defn s0 []
  (->> "./src/day-5/input.txt"
       (slurp)
       (str/split-lines)
       (map id)
       (apply max)))

(defn s1 []
  (let [bs (->> "./src/day-5/input.txt"
                (slurp)
                (str/split-lines)
                (map id)
                (set))
        ms (set/difference (set (range 0 902)) bs)]
    (first (filter #(and (contains? bs (+ % 1))
                         (contains? bs (- % 1))) ms))))

(defn -main [& args]
  (println "Day 5, Part 1")
  (println (time (s0)))
  (println "Day 5, Part 2")
  (println (time (s1))))
