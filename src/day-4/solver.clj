#!/usr/bin/env boot

(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn pe [in]
  (as-> in v
       (str/split-lines v)
       (map str/trim v)
       (str/join " " v)
       (str/split v #" ")
       (map #(str/split % #":") v)
       (into {} v)))

(defn v0? [p]
  (let [rf (set '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))]
    (set/subset? rf (set (keys p)))))

(defn s0 []
  (as-> "./src/day-4/input.txt" v
       (slurp v)
       (str/split v #"\n\n")
       (map pe v)
       (filter v0? v)
       (count v)))

(defn v1? [p]
  (let [byr (read-string (get p "byr"))
        iyr (read-string (get p "iyr"))
        eyr (read-string (get p "eyr"))
        hgt (as-> p v
              (get v "hgt")
              (str/split v #"(?<=[0-9])(?=[a-z])")
              (map vector [:v :u] v)
              (into {} v)
              (update v :v read-string))
        hcl (get p "hcl")
        ecl (get p "ecl")
        pid (get p "pid")]
    (and (>= byr 1920)
         (<= byr 2002)
         (>= iyr 2010)
         (<= iyr 2020)
         (>= eyr 2020)
         (<= eyr 2030)
         (cond
           (= (:u hgt) "cm") (and (>= (:v hgt) 150)
                                  (<= (:v hgt) 193))
           :else             (and (>= (:v hgt) 59)
                                  (<= (:v hgt) 76)))
         (some? (re-matches #"\#([0-9a-f]){6}" hcl))
         (contains? (set '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")) ecl)
         (some? (re-matches #"([0-9]){9}" pid)))))

(defn s1 []
  (as-> "./src/day-4/input.txt" v
       (slurp v)
       (str/split v #"\n\n")
       (map pe v)
       (filter v0? v)
       (filter v1? v)
       (count v)))

(defn -main [& args]
  (println "Day 4, Part 1")
  (println (time (s0)))
  (println "Day 4, Part 2")
  (println (time (s1))))
