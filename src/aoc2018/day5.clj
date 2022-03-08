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

(defn react
  "새로운 폴리머가 이미 반응이 완료한 결과물의 마지막 폴리머와 상극이고 타입이 같은 폴리머일 경우
  그 둘이 반응해 직전 결과물의 마지막 폴리머를 삭제하고 아니라면 새로운 폴리머를 결과물 뒤에 추가한다."
  [reacted new-polymer]
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
