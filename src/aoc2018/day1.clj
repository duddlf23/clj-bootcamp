(ns aoc2018.day1
  "https://adventofcode.com/2018/day/1"
  (:require [util.file :as file]))

(def input (file/read-edn "aoc2018/day1_in.edn"))

; Part 1

(->> input
     (reduce +))

; Part 2

(->> input
     cycle
     (reductions +)
     (reduce (fn [sums-history current-sum]
               (if (contains? sums-history current-sum)
                 (reduced current-sum)
                 (conj sums-history current-sum)))
             #{}))
