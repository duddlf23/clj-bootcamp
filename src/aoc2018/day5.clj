(ns aoc2018.day5
  "https://adventofcode.com/2018/day/5"
  (:require [util.file :as file]
            [clojure.string :as str]))

(def input-file "aoc2018/day5_in.edn")
(def input (file/read-edn input-file))

(def upper-case-letters (map char (range 65 91)))
(def lower-case-letters (map char (range 97 123)))

(def upper->lower (zipmap upper-case-letters lower-case-letters))
(def lower->upper (zipmap lower-case-letters upper-case-letters))
(def change-case-map (merge upper->lower lower->upper))

; change-letter-case-map을 직접 함수로 써도 되나 해시 맵을 데이터라고 봤을 때 get을 쓰는게 좀 더 자연스럽다..?
(defn change-case [letter]
  (get change-case-map letter))

; Part 1
; react
(defn react [acc-polymers new-polymer]
  (if (= (change-case new-polymer) (peek acc-polymers))
    (pop acc-polymers)
    (conj acc-polymers new-polymer)))

(defn alchemical-reduce [polymers-str]
  (reduce react
          []
          polymers-str))

(defn alchemical-reduce-with-atom
  "reduce를 안 써보려 하다가 등장한 괴물이다.
  문제 특성상 reduce 과정이 fold-left의 형식이므로 병렬성도 보장이 안될텐데 atom을 써서 얻을 수 있는 이점이 잘 안 보인다.
  그냥 써본 정도인가..?"
  [polymers-str]
  (let [reduced (atom [])]
    (run!
      (partial swap! reduced react)
      polymers-str)
    @reduced))

(comment
  (= (last [\a \b]) (change-case \B))
  (react [\a \e \f] \F)
  (alchemical-reduce "dabAcCaCBAcCcaDA")
  (-> input
      alchemical-reduce
      count)
  (-> input
      alchemical-reduce-with-atom
      count))

; Part 2
(defn get-distinct-letters-case-insensitive [s]
  (-> s
      str/lower-case
      distinct))

(defn remove-letter-case-insensitive [s letter]
  (->> s
       (remove #{letter (change-case letter)})
       (apply str)))

(comment
  (get-distinct-letters-case-insensitive "AdeAEdbFyh")
  (remove-letter-case-insensitive "AdeAEdbFyh" \a)

  (->> (get-distinct-letters-case-insensitive input)
       (map (partial remove-letter-case-insensitive input))
       (map (comp count alchemical-reduce))
       (apply min)))
