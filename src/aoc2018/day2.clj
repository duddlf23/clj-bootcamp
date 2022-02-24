(ns aoc2018.day2
  "https://adventofcode.com/2018/day/2"
  (:require [util.file :as file]
            [clojure.string :as string]))

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


((partial filter odd?) [1 3])


((filter odd?) [1 3])

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

(comment
  (count-diff "abb" "aaa")
  (get-longest-matched-str "aaa" "aba")
  (find-common-letters input))

