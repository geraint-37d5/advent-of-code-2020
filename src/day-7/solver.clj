#!/usr/bin/env boot

(require '[clojure.string :as str])

(defn gmp [in]
  (as-> in v
    (str/split-lines v)
    (mapv #(str/split % #" bags contain ") v)
    (mapv
     #(vector (first %)
              (as-> (second %) cs
                (str/replace cs #"bag(s){0,1}(\.){0,1}" "")
                (str/trim cs)
                (str/split cs #", ")
                (mapv (fn [v'] (str/split v' #"(?<=[0-9]+)")) cs)
                (mapv (fn [v'] (mapv str/trim v')) cs)
                (mapv (fn [v'] (vector (second v') (first v'))) cs)
                (into {} cs))) v)
    (into {} v)))

(defn csb [k mp]
  (let [m' (get mp k)]
    (cond
      (some? (get m' "shiny gold" nil)) true
      (some? (get m' nil nil)) false
      :else (some? (some true? (map #(csb % mp) (keys m')))))))

(defn s0 [in]
  (let [mp (gmp in)]
    (as-> mp v
      (map #(csb % mp) (keys v))
      (filter true? v)
      (count v))))

(defn csb' [k mp]
  (let [m' (get mp k)]
    (cond
      (some? (get m' nil nil)) [1]
      :else (map #(let [k' (first %)
                        v  (read-string (second %))
                        ne (not (some? (get (get mp k') nil nil)))]
                    (+ (cond
                         (true? ne) v
                         :else 0)
                       (* (reduce + (csb' k' mp)) v))) m'))))

(defn s1 [in]
  (let [mp (gmp in)]
    (as-> mp v
      (csb' "shiny gold" v)
      (reduce + v))))

(defn -main [& args]
  (let [in (slurp "./src/day-7/input.txt")]
    (println "Day 7, Part 1")
    (println (time (s0 in)))
    (println "Day 7, Part 2")
    (println (time (s1 in)))))
