#!/usr/bin/env boot

(require '[clojure.string :as str])

(def vs
  (->> "./src/day-1/input.txt"
       (slurp)
       (str/split-lines)
       (mapv read-string)))

;; Part 1
(defn r0 [tv acc next]
  (cond
    (= 2020 (+ tv next)) (reduced [true (* tv next)])
    :else [false]))

(defn s0 []
  (->> vs
       (mapv #(reduce (partial r0 %) 0 vs))
       (filter #(true? (first %)))
       (first)
       (last)))

;; Part 2
(defn r1 [tv acc next]
  (cond
    (= 2020 (+ (first tv) next)) (reduced [true (* (nth tv 1) (nth tv 2) next)])
    :else [false]))

(defn s1 []
  (->> vs
       (mapv #(mapv (fn [v] [(+ v %) v %]) vs))
       (mapv #(filterv (fn [v] (< (first v) 2020)) %))
       (filterv not-empty)
       (mapcat identity)
       (mapv #(reduce (partial r1 %) 0 vs))
       (filter #(true? (first %)))
       (first)
       (last)))

(defn -main [& args]
  (println "Day 1, Part 1")
  (println (time (s0)))
  (println "Day 1, Part 2")
  (println (time (s1))))
