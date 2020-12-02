#!/usr/bin/env boot

(require '[clojure.string :as str])

(defn pl [s]
  (let [ts (str/split s #":")
        t0 (first ts)
        t1 (str/trim (last ts))
        tsp (str/split t0 #" ")
        tsc (last tsp)
        tsm (str/split (first tsp) #"-")
        spec {:min (read-string (first tsm))
              :max (read-string (second tsm))
              :c (first tsc)}]
    [spec t1]))

(defn v0? [{ min :min max :max c :c } ws]
  (as-> ws v
       (vec v)
       (group-by identity v)
       (map #(vec [(first %) (count (second %))]) v)
       (into {} v)
       (get v c false)
       (and (not (false? v)) (>= v min) (<= v max))))

(defn s0 []
  (->> "./src/day-2/input.txt"
       (slurp)
       (str/split-lines)
       (map pl)
       (filter #(apply v0? %))
       (count)))

(defn v1? [{ min :min max :max c :c } ws]
  (as-> ws v
    (vec v)
    (or
     (and (= (nth v (- min 1)) c)
          (not (= (nth v (- max 1)) c)))
     (and (= (nth v (- max 1)) c)
          (not (= (nth v (- min 1)) c))))))

(defn s1 []
  (->> "./src/day-2/input.txt"
       (slurp)
       (str/split-lines)
       (map pl)
       (filter #(apply v1? %))
       (count)))

(defn -main [& args]
  (println "Day 2, Part 1")
  (println (time (s0)))
  (println "Day 2, Part 2")
  (println (time (s1))))
