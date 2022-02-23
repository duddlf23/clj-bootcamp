(ns aoc2018.day2
  "https://adventofcode.com/2018/day/2"
  (:require [util.file :as file]))

(def input (file/read-edn "aoc2018/day2_in.edn"))

; Part 1

(def checksum-freq-conditions [#(contains? % 2) #(contains? % 3)])

(defn get-freq-nums-set [id]
  (-> id
      frequencies
      vals
      set))

(-> (map get-freq-nums-set input)
    (as-> freq-info
          (map #(-> %
                  (filter freq-info)
                  count) checksum-freq-conditions))
    (->> (reduce *)))

; Part 2
(defn count-diff [xs ys]
  (->> ys
      (map = xs)
      (filter false?)
      count))

(defn get-lcs [xs ys]
  (->> ys
       (map = xs)))

(reduce)
(count-diff "aaa" "abb")


