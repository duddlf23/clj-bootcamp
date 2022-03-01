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

(defn get-all-fabric-coordinate-seq [claims]
    (->> claims
         (mapcat get-fabric-coordinates)))

(defn get-fabric-frequencies [claims]
  (->> claims
       get-all-fabric-coordinate-seq
       frequencies))

(defn get-overlap-count [fabric-frequencies]
  (->> fabric-frequencies
       vals
       (filter #(> % 1))
       count))

(comment
  (->> (parse-one-line-to-hash-map "#1 @ 12,548: 19x10")
       get-fabric-coordinates
       frequencies)
  (get-input-claim-seq "aoc2018/day3_in_test.txt")
  (->> "aoc2018/day3_in_test.txt"
       get-input-claim-seq
       get-fabric-frequencies))

(comment
  (-> input-file
      get-input-claim-seq
      get-fabric-frequencies
      get-overlap-count))

; Part 2

;; Use Part 1 functions
(defn find-not-overlap-claim [claims]
  (let [fabrics-frequencies (get-fabric-frequencies claims)
        exactly-once? #(= 1 (get fabrics-frequencies %))
        not-overlap? (fn [claim]
                       (->> claim
                            get-fabric-coordinates
                            (every? exactly-once?)))]
    (->> claims
         (filter not-overlap?)
         first)))

(comment
  (-> input-file
      get-input-claim-seq
      find-not-overlap-claim
      :id))


;;; Compare coordinates range
;(defn not-overlap-range?
;  "시작점(*-start)은 inclusive하고, 끝점(*-end)은 exclusive하다."
;  [range1-start range1-end range2-start range2-end]
;  (or (<= range1-end range2-start)
;      (>= range1-start range2-end)))
;
;(defn not-overlap?
;  "두 클레임의 x 좌표 범위나 y 좌표 범위가 서로 겹치는 부분이 없을 경우 오버랩하지 않는다."
;  [claim1 claim2]
;  (let [{start-x1 :start-x start-y1 :start-y end-x1 :end-x end-y1 :end-y} claim1
;        {start-x2 :start-x start-y2 :start-y end-x2 :end-x end-y2 :end-y} claim2]
;    (or (not-overlap-range? start-x1 end-x1 start-x2 end-x2)
;        (not-overlap-range? start-y1 end-y1 start-y2 end-y2))))
;
;(defn same-claims?
;  "두 클레임의 id가 서로 같은지 검사"
;  [{id1 :id}
;   {id2 :id}]
;  (= id1 id2))
;
;(defn find-not-overlap-claim-id-v2 [claims]
;  (->> (for [current-claim claims
;             :let [not-overlap-with-current-claim? (partial not-overlap? current-claim)
;                   is-current-claim? (partial same-claims? current-claim)]
;             :when (every? #(or (is-current-claim? %)
;                                (not-overlap-with-current-claim? %)) claims)]
;         (:id current-claim))
;       first))
;
;
;(comment
;  (def claims (get-input-claim-seq input-file))
;  (not-overlap-range? 1 2 2 5)
;  (find-not-overlap-claim-id-v2 claims))


