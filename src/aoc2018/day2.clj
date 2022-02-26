(ns aoc2018.day2
  "https://adventofcode.com/2018/day/2"
  (:require [util.file :as file]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(def input (file/read-edn "aoc2018/day2_in.edn"))

; Part 1
(defn get-freq-nums-set [id]
  (->> id
       frequencies
       vals
       set))

(defn get-checksum [ids]
  (let [contains-n-filter (fn [n]
                            (partial filter #(contains? % n)))
        checksum-freq-filters (juxt
                                (contains-n-filter 2)
                                (contains-n-filter 3))]
    (->> ids
         (map get-freq-nums-set)
         checksum-freq-filters
         (map count)
         (reduce *))))


(comment
  (get-freq-nums-set (nth input 0))
  (get-checksum input))


; Part 2
(defn count-diff
  "두 스트링의 각 문자를 순서대로 비교해 서로 다른 문자의 개수"
  [xs ys]
  (->> (map not= xs ys)
       (filter true?)
       count))

(defn get-longest-matched-str
  "두 스트링의 각 문자를 순서대로 비교해 서로 같은 모든 문자들의 부분 스트링"
  [xs ys]
  (->> (map vector xs ys)
       (keep (fn [[x y]]
               (when (= x y) x)))
       string/join))

(defn find-common-letters [box-ids]
  (reduce (fn [accumulated current-id]
            (let [target-id
                  (->> accumulated
                       (some #(when (= 1 (count-diff current-id %)) %)))]
              (if (nil? target-id)
                (conj accumulated current-id)
                (reduced (get-longest-matched-str current-id target-id)))))
          []
          box-ids))

(defn find-common-letters-with-combi [box-ids]
  (let [allowed-diff-condition #(= 1 (count-diff %1 %2))]
    (->> (combo/combinations box-ids 2)
         (some (fn [[x y]]
                 (when (allowed-diff-condition x y)
                   (get-longest-matched-str x y)))))))

; 리뷰 반영

(defn find-common-letters-using-for [box-ids]
  (let [allowed-diff-condition #(= 1 (count-diff %1 %2))]
    (->> (for [id1 box-ids
               id2 box-ids
               :when (allowed-diff-condition id1 id2)]
           (get-longest-matched-str id1 id2))
         first)))

(defn find-common-letters-using-for-and-index [box-ids]
  (let [box-ids-with-index (map vector box-ids (range))
        allowed-diff-condition #(= 1 (count-diff %1 %2))]
    (->> (for [[id1 id1-idx] box-ids-with-index
               [id2 id2-idx] box-ids-with-index
               :while (> id1-idx id2-idx)
               :when (allowed-diff-condition id1 id2)]
           (get-longest-matched-str id1 id2))
         first)))

(comment
  (count-diff "abb" "aaa")
  (get-longest-matched-str "aaa" "aba")
  (find-common-letters input)
  (find-common-letters-with-combi input)
  (find-common-letters-using-for input)
  (find-common-letters-using-for-and-index input))

