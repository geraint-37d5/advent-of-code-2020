(require '[clojure.string :as str])
(require '[java.lang.integer :as int])

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

(->> vs
     (mapv #(reduce (partial r0 %) 0 vs))
     (filter #(true? (first %)))
     (first))

;; Part 2
(defn r1 [tv acc next]
  (cond
    (= 2020 (+ (first tv) next)) (reduced [true (* (nth tv 1) (nth tv 2) next)])
    :else [false]))

(->> vs
     (mapv #(mapv (fn [v] [(+ v %) v %]) vs))
     (mapv #(filterv (fn [v] (< (first v) 2020)) %))
     (filterv not-empty)
     (mapcat identity)
     (mapv #(reduce (partial r1 %) 0 vs))
     (filter #(true? (first %)))
     (first))
