(ns aoc2018.day2
  "https://adventofcode.com/2018/day/2"
  (:require [util.file :as file]
            [clojure.string :as string]))

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
       (map (fn [x y]
              (if (= x y)
                x
                ""))
            xs)
       string/join))

(reduce (fn [accumulated current]
          (let [target
                (->> accumulated
                     (some #(when (= 1 (count-diff current %)) %)))]
            (if (nil? target)
              (conj accumulated current)
              (reduced (get-lcs current target)))))
        []
        input)
