(ns aoc2020.day1
  "https://adventofcode.com/2020/day/1"
  (:require [util.file :as file]))

(def input (set (file/read-edn "aoc2020/day1_in.edn")))

; Part 1

(defn find-two-sum [nums-set target-sum]
  (-> (for [num nums-set
            :let [required-2nd-num (- target-sum num)]
            :when ((disj nums-set num) required-2nd-num)]
        [num required-2nd-num])
      first))

(comment
  (->> (find-two-sum input 2020)
       (apply *)))

; Part 2

(defn find-three-sum [nums-set target-sum]
  (-> (for [num nums-set
            :let [remain-two-sum (find-two-sum (disj nums-set num) (- target-sum num))]
            :when remain-two-sum]
        (conj remain-two-sum num))
      first))

(comment
  (->> (find-three-sum input 2020)
       (apply *)))