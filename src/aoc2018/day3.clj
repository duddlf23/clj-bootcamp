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

(defn parse-one-line-to-hash-map
  "start-x, start-y : inclusive
   end-x, end-y : exclusive"
  [line]
  (->> line
       parse-one-line-to-seq
       ((fn [[id start-x start-y w h]]
          {:id      id
           :start-x start-x
           :start-y start-y
           :end-x   (+ start-x w)
           :end-y   (+ start-y h)}))))

(comment
  (parse-one-line-to-seq "#1 @ 12,548: 19x10")
  (parse-one-line-to-hash-map "#1 @ 12,548: 19x10"))

(defn get-input-claim-seq [file-name]
  (->> (file/read-file file-name)
       (map parse-one-line-to-hash-map)))


; Part 1

(defn get-fabric-coordinates [{:keys [start-x start-y end-x end-y]}]
  (for [x (range start-x end-x)
        y (range start-y end-y)]
    [x y]))

(defn get-all-fabric-coordinate-seq [file-name]
    (->> file-name
         get-input-claim-seq
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
  (get-input-claim-seq "aoc2018/day3_in_test.txt")
  (get-all-fabric-coordinate-seq "aoc2018/day3_in_test.txt"))

(comment
  (->> (get-all-fabric-coordinate-seq input-file)
       get-overlap-count))

; Part 2

(defn not-overlap-range?
  "시작점(*-start)은 inclusive하고, 끝점(*-end)은 exclusive하다."
  [range1-start range1-end range2-start range2-end]
  (or (<= range1-end range2-start)
      (>= range1-start range2-end)))

(defn not-overlap?
  "두 클레임의 id가 같은 경우도 오버랩하지 않는 경우라 생각함
  두 클레임의 x 좌표 범위나 y 좌표 범위가 서로 겹치는 부분이 없을 경우 오버랩하지 않는다."
  [claim1 claim2]
  (let [{id1 :id start-x1 :start-x start-y1 :start-y end-x1 :end-x end-y1 :end-y} claim1
        {id2 :id start-x2 :start-x start-y2 :start-y end-x2 :end-x end-y2 :end-y} claim2]
    (or (= id1 id2)
        (not-overlap-range? start-x1 end-x1 start-x2 end-x2)
        (not-overlap-range? start-y1 end-y1 start-y2 end-y2))))

(defn find-not-overlap-claim-id [claims]
  (->> (for [claim claims
             :let [not-overlap-with-current-claim? (partial not-overlap? claim)]
             :when (every? not-overlap-with-current-claim? claims)]
         (:id claim))
       first))


(comment
  (def claims (get-input-claim-seq input-file))
  (not-overlap-range? 1 2 2 5)
  (find-not-overlap-claim-id claims))


