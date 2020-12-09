#!/usr/bin/env boot

(require '[clojure.string :as str])

;; Part 1

(defn pi [in]
  (->> in
       (str/split-lines)
       (mapv read-string)))

(defn is2 [[v ps]]
  (let [ps' (set ps)]
    (->> ps
         (map #(contains? ps' (- v %)))
         (some true?))))

(defn fm [ds ps]
  (->> ds
    (drop ps)
    (map-indexed #(vector %2 (subvec ds %1 (+ %1 ps))))
    (map #(vector (is2 %) %))
    (filter #(nil? (first %)))
    (first)
    (second)
    (first)))

;; Part 2

(defn twv [[tn vs]]
  (reduce #(let [tn' (- (first %1) %2)]
             (cond
               (>= tn' 0) [tn' (conj (second %1) %2)]
               :else (reduced %1))) [tn []] vs))

(defn fcs [ds tn]
  (let [ds' (vec (reverse (filter #(< % tn) ds)))]
    (->> (range 0 (count ds'))
         (map #(vector tn (subvec ds' %)))
         (map twv)
         (filter #(= (first %) 0))
         (first)
         (second)
         (#(+ (apply min %) (apply max %))))))

(defn -main [& args]
  (let [in (pi (slurp "./src/day-9/input.txt"))]
    (println "Day 9, Part 1")
    (println (time (fm in 25)))
    (println "Day 9, Part 2")
    (println (time (fcs in 27911108)))))
