(ns aoc2018.day3
  "https://adventofcode.com/2018/day/3"
  (:require [util.file :as file]
            [clojure.edn :as edn]))

(def input-file "aoc2018/day3_in.txt")

; part 1
(defn parse-one-line-to-seq [line]
  (->> line
       (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")
       (drop 1)
       (map edn/read-string)))

(defn parse-one-line-to-hash-map [line]
  (->> line
       parse-one-line-to-seq
       ((fn [[id start-x start-y length-x length-y]]
          {:id id
           :start-x start-x
           :start-y start-y
           :length-x length-x
           :length-y length-y}))))

(comment
  (parse-one-line-to-seq "#1 @ 12,548: 19x10")
  (parse-one-line-to-hash-map "#1 @ 12,548: 19x10"))

(defn get-input-xy-seq [file-name]
  (->> (file/read-file file-name)
       (map parse-one-line-to-hash-map)))

(defn get-fabric-coordinates [{:keys [start-x start-y length-x length-y]}]
  (for [dx (range length-x)
        dy (range length-y)]
    [(+ start-x dx) (+ start-y dy)]))

(defn get-all-fabric-coordinate-seq [file-name]
    (->> file-name
         get-input-xy-seq
         (mapcat get-fabric-coordinates)))

(defn get-overlap-count [fabric-coordinates]
  (->> fabric-coordinates
       frequencies
       vals
       (filter #(> % 1))
       count))

(comment
  (for [dx (range 5)
        dy (range 6)]
    [(+ dx 3) (+ dy 5)])
  (get-input-xy-seq "aoc2018/day3_in_test.txt")
  (get-all-fabric-coordinate-seq "aoc2018/day3_in_test.txt"))

(comment
  (->> (get-all-fabric-coordinate-seq input-file)
       get-overlap-count))
