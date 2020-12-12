#!/usr/bin/env boot

(merge-env! :dependencies '[[org.clojure/core.match "1.0.0"]])

(require '[clojure.string :as str])
(require '[clojure.core.match :refer [match]])

(defn pi [in]
  (->> in
       (str/split-lines)
       (map read-string)))

(defn cpr [in]
  (->> in
       (vec)
       (#(conj % (+ 3 (apply max %))))
       (sort)
       (reduce #(vector %2 (conj (second %1) (- %2 (first %1)))) '(0, []))
       (second)
       (group-by identity)
       (reduce-kv #(assoc %1 %2 (count %3)) {})
       (#(* (get % 1 0) (get % 3 0)))))

(defn s0 [in]
  (->> in
       (pi)
       (cpr)))

(defn cds [in]
  (->> in
       (vec)
       (#(conj % (+ 3 (apply max %))))
       (sort)
       (reduce #(vector %2 (conj (second %1) (- %2 (first %1)))) '(0, []))
       (second)))

(defn s1 [in]
  (->> in
       (pi)
       (sort)
       (cds)
       (map str)
       (str/join)
       (#(str/split % #"3"))
       (map count)
       (filter #(> % 1))
       (map #(match [%]
                    [2] 2
                    [3] 4
                    :else (- (* % 2) 1)))
       (reduce *)))

(defn -main [& args]
  (let [in (slurp "./src/day-10/input.txt")]
    (println "Day 10, Part 1")
    (println (time (s0 in)))
    (println "Day 10, Part 2")
    (println (time (s1 in)))))
