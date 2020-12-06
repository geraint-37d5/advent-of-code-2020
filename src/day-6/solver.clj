#!/usr/bin/env boot

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn s0 [in]
  (as-> in v
    (str/split v #"\n\n")
    (map str/split-lines v)
    (map #(map set %) v)
    (map #(reduce set/union %) v)
    (map count v)
    (reduce + v)))

(defn s1 [in]
  (as-> in v
    (str/split v #"\n\n")
    (map str/split-lines v)
    (map #(map set %) v)
    (map #(reduce set/intersection %) v)
    (map count v)
    (reduce + v)))

(defn -main [& args]
  (let [in (slurp "./src/day-6/input.txt")]
    (println "Day 5, Part 1")
    (println (time (s0 in)))
    (println "Day 5, Part 2")
    (println (time (s1 in)))))
