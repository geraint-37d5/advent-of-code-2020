#!/usr/bin/env boot

(merge-env! :dependencies '[[org.clojure/core.match "1.0.0"]])

(require '[clojure.string :as str])
(require '[clojure.data :as data])
(require '[clojure.core.match :refer [match]])

;; Part 1

(def compass [[1 0] [1 -1] [1 1] [-1 0] [-1 1] [-1 -1] [0 1] [0 -1]])

(defn parse-grid [in]
  (as-> in pv
    (str/split-lines pv)
    (map #(map-indexed vector %) pv)
    (map-indexed (fn [y xs] (map #(vector [(first %) y] (second %)) xs)) pv)
    (mapcat identity pv)
    (into {} pv)))

(defn get-adj-cells [[x y] grid]
  (map (fn [[x' y']] (get grid [(+ x x') (+ y y')] nil)) compass))

(defn count-pred [pred coll]
  (->> coll
       (map pred)
       (filter true?)
       (count)))

(defn cell-empty? [cell]
  (= cell \L))

(defn cell-occupied? [cell]
  (= cell \#))

(defn cell-next-state [[x y] grid]
  (let [cell               (get grid [x y])
        adj-cells          (get-adj-cells [x y] grid)
        adj-empty-count    (count-pred cell-empty? adj-cells)
        adj-occupied-count (count-pred cell-occupied? adj-cells)]
    (match [cell adj-occupied-count]
           [\L 0] \#
           [\# (_ :guard #(>= % 4))] \L
           :else cell)))

(defn grid-next-state [grid]
  (->> grid
       (seq)
       (map #(vector (first %) (cell-next-state (first %) grid)))
       (into {})))

(defn grid-run-until-stable [iter lim grid]
  (let [grid' (grid-next-state grid)
        diff  (data/diff grid grid')]
    (cond
      (>= iter lim)        [iter grid']
      (some? (first diff)) (grid-run-until-stable (+ iter 1) lim grid')
      :else                [iter grid'])))

(defn grid-count-occupied [grid]
  (count-pred true? (map cell-occupied? (vals grid))))

(defn solve-p1 [in]
  (->> in
       (parse-grid)
       (grid-run-until-stable 0 1000)
       (second)
       (grid-count-occupied)))

;; Part 2

(defn cell-search-los [[x y] [x' y'] grid]
  (let [target-coord [(+ x x') (+ y y')]
        target-cell  (get grid target-coord nil)]
    (match [target-cell]
           [\.] (cell-search-los target-coord [x' y'] grid)
           :else target-cell)))

(defn cell-get-all-los [[x y] grid]
  (map #(cell-search-los [x y] % grid) compass))

(defn cell-next-state-with-los [[x y] grid]
  (let [cell               (get grid [x y])
        los-cells          (cell-get-all-los [x y] grid)
        los-occupied-count (count-pred cell-occupied? los-cells)]
    (match [cell los-occupied-count]
           [\L 0] \#
           [\# (_ :guard #(>= % 5))] \L
           :else cell)))

(defn grid-next-state-with-los [grid]
  (->> grid
       (seq)
       (map #(vector (first %) (cell-next-state-with-los (first %) grid)))
       (into {})))

(defn grid-run-until-stable-with-los [iter lim grid]
  (let [grid' (grid-next-state-with-los grid)
        diff  (data/diff grid grid')]
    (cond
      (>= iter lim)        [iter grid']
      (some? (first diff)) (grid-run-until-stable-with-los (+ iter 1) lim grid')
      :else                [iter grid'])))

(def raw-input
  (slurp "./src/day-11/input.txt"))

(defn solve-p2 [in]
  (->> in
       (parse-grid)
       (grid-run-until-stable-with-los 0 1000)
       (second)
       (grid-count-occupied)))

(defn -main [& args]
  (let [in (slurp "./src/day-11/input.txt")]
    (println "Day 11, Part 1")
    (println (time (solve-p1 in)))
    (println "Day 11, Part 2")
    (println (time (solve-p2 in)))))
