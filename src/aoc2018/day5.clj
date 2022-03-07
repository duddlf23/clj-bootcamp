(ns aoc2018.day5
  "https://adventofcode.com/2018/day/5"
  (:require [clojure.string :as str]
            [util.file :as file]))

(def input-file "aoc2018/day5_in.edn")
(def input (file/read-edn input-file))

(def upper-units (seq "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(def lower-units (seq "abcdefghijklmnopqrstuvwxyz"))

(def upper->lower (zipmap upper-units lower-units))
(def lower->upper (zipmap lower-units upper-units))
(def change-polarity (merge upper->lower lower->upper))

; Part 1
; react
(defn react [reacted new-polymer]
  (if (= (change-polarity new-polymer) (peek reacted))
    (pop reacted)
    (conj reacted new-polymer)))

(defn alchemical-reduce [polymers]
  (reduce react
          []
          polymers))

(comment
  (= (last [\a \b]) (change-polarity \B))
  (react [\a \e \f] \F)
  (alchemical-reduce "dabAcCaCBAcCcaDA")

  (-> input
      alchemical-reduce
      count))

; Part 2
(defn get-distinct-units [polymers]
  (-> polymers
      str/lower-case
      distinct))

(defn remove-unit [polymers unit]
  (remove #{unit (change-polarity unit)} polymers))

(comment
  (get-distinct-units "AdeAEdbFyh")
  (remove-unit "AdeAEdbFyh" \a)

  (->> (get-distinct-units input)
       (map #(remove-unit input %))
       (map #(count (alchemical-reduce %)))
       (apply min)))
