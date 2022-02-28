(ns aoc2018.day4
  "https://adventofcode.com/2018/day/4"
  (:require [util.file :as file]))

(def input-file "aoc2018/day4_in.txt")

(defn parse-int [s]
  (if s (Integer/parseInt s) nil))

(defn parse-one-log
  "로그를 파싱해 문제에 필요한 요소인 가드 id와 액션이 일어난 분만 캐치함"
  [line]
  (->> line
       (re-find #".*:(\d+)[^0-9]*(\d+)?")
       (drop 1)
       (map parse-int)
       ((fn [[minute guard-id]]
          {:minute minute
           :guard-id guard-id}))))

(defn parse-input-logs [file-name]
  (->> (file/read-file file-name)
       sort
       (map parse-one-log)))

(defn is-begins-shift-log? [log]
  (->> log
       :guard-id
       some?))

(defn get-last-guard-id
  "교대 근무 로그만 연속으로 모여 있을 경우 마지막 근무 가드의 id만 가져옴"
  [begins-shift-logs]
  (->> begins-shift-logs
       last
       :guard-id))

(defn separate-log-to-guards-actions
  "파싱된 로그들을 입력받아 가드마다의 액션 타임(분)을 집계
  포맷: {:guard-id guard-id
        :actions (asleep awake asleep awake ...)}
  ex) [{:guard-id 877
        :actions (6 25 38 51 ...)},
       {:guard-id 1021
        :actions (15 34 37 44 ...)}, ...]"
  [logs]
  (->> logs
       (partition-by is-begins-shift-log?)
       (partition 2)
       (map (fn [[begins-shift-logs actions]]
              {(get-last-guard-id begins-shift-logs) (map :minute actions)}))
       (apply merge-with concat)
       (map (fn [[guard-id actions]]
              {:guard-id guard-id
               :actions actions}))))

(comment
  (->> input-file
       parse-input-logs
       separate-log-to-guards-actions))

(defn get-total-asleep-times [actions]
  (->> actions
       (partition 2)
       (map #(- (second %) (first %)))
       (reduce +)))

(defn get-most-asleep-minute-portion
  "액션들을 입력받아 가장 많이 잤던 분을 찾아 빈도수와 함께 리턴
  ex) {:minute 30
       :freq 5}"
  [actions]
  (->> actions
       (partition 2)
       (map #(range (first %) (second %)))
       (reduce concat)
       frequencies
       (apply max-key val)
       ((fn [[minute freq]]
          {:minute minute
           :freq freq}))))

; Part 1

(defn find-most-asleep-guard-actions [guards-actions]
  (->> guards-actions
       (apply max-key #(get-total-asleep-times (:actions %)))))

(comment
  (->> (parse-input-logs input-file)
       separate-log-to-guards-actions
       find-most-asleep-guard-actions
       ((fn [{:keys [guard-id actions]}]
          (->> actions
               get-most-asleep-minute-portion
               :minute
               (* guard-id))))))

; Part 2
(defn map-most-asleep-minute-per-guard [guards-actions]
  (->> guards-actions
       (map (fn [{:keys [guard-id actions]}]
              (-> actions
                  get-most-asleep-minute-portion
                  (assoc :guard-id guard-id))))))

(defn find-most-frequently-asleep-same-minute [guard-id->action-minutes]
  (->> guard-id->action-minutes
       map-most-asleep-minute-per-guard
       (apply max-key :freq)))

(comment
  (->> (parse-input-logs input-file)
       separate-log-to-guards-actions
       find-most-frequently-asleep-same-minute
       ((fn [{:keys [minute guard-id]}]
          (* minute guard-id)))))
