#!/usr/bin/env boot

(merge-env! :dependencies '[[org.clojure/core.match "1.0.0"]])

(require '[clojure.string :as str])
(require '[clojure.core.match :refer [match]])

(def is {:v 0 :j 1})

(defn nop [arg st]
  (-> st
   (assoc :j 1)))
(defn acc [arg st]
  (-> st
      (assoc :j 1)
      (update :v + arg)))
(defn jmp [arg st]
  (-> st
      (assoc :j arg)))

(defn pi [in]
  (->> in
       (str/split-lines)
       (mapv #(str/split % #" "))
       (mapv #(vector (first %) (read-string (second %))))))

(defn r0 [cs ns pt sp]
  (let [pt'      (+ pt (:j ns))
       [act arg] (nth cs pt' nil)]
    (match [sp act]
           [(_ :guard #(contains? % pt')) _] ns
           [_ nil] ns
           [_ "acc"] (r0 cs (acc arg ns) pt' (conj sp pt'))
           [_ "nop"] (r0 cs (nop arg ns) pt' (conj sp pt'))
           [_ "jmp"] (r0 cs (jmp arg ns) pt' (conj sp pt'))
           :else nil)))

(defn s0 [in]
  (-> in
      (pi)
      (r0 is -1 #{})))

(defn r1 [cs ns pt sp]
  (let [pt'      (+ pt (:j ns))
       [act arg] (nth cs pt' nil)]
    (match [sp act]
           [(_ :guard #(contains? % pt')) _] [false ns]
           [_ nil] [true ns]
           [_ "acc"] (r1 cs (acc arg ns) pt' (conj sp pt'))
           [_ "nop"] (r1 cs (nop arg ns) pt' (conj sp pt'))
           [_ "jmp"] (r1 cs (jmp arg ns) pt' (conj sp pt'))
           :else nil)))

(defn sims [cs]
  (->> cs
       (mapv #(vector %1 %2) (range))
       (filterv (fn [[_ [act _]]] (or (= act "nop") (= act "jmp"))))
       (mapv (fn [[idx [act arg]]]
               (match [act]
                      ["nop"] [idx ["jmp" arg]]
                      ["jmp"] [idx ["nop" arg]])))
       (mapv (fn [[idx cmd]] (assoc cs idx cmd)))))

(defn s1 [in]
  (->> in
      (pi)
      (sims)
      (pmap #(r1 % is -1 #{}))
      (reduce #(cond (true? (first %2)) (reduced %2) :else %2) false)))

(defn -main [& args]
  (let [in (slurp "./src/day-8/input.txt")]
    (println "Day 8, Part 1")
    (println (time (s0 in)))
    (println "Day 8, Part 2")
    (println (time (s1 in)))))
