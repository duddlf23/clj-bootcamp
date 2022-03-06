(ns aoc2018.day4
  "https://adventofcode.com/2018/day/4"
  (:require [util.file :as file]))

(defn parse-int [s]
  (when s (Integer/parseInt s)))

(defn parse-one-log
  "로그를 파싱해 문제에 필요한 요소인 가드 id와 액션이 일어난 분만 캐치함"
  [line]
  (let [[_ minute guard-id] (re-find #".*:(\d+)[^0-9]*(\d+)?" line)]
    {:minute (parse-int minute)
     :guard-id (parse-int guard-id)}))

(def input (->> (file/read-file "aoc2018/day4_in.txt")
                sort
                (map parse-one-log)))

(defn begins-shift-log? [log]
  (-> log
      :guard-id
      some?))

(defn get-last-guard-id
  "교대 근무 로그만 연속으로 모여 있을 경우 마지막 근무 가드의 id만 가져옴"
  [begins-shift-logs]
  (-> begins-shift-logs
      last
      :guard-id))

(defn actions->asleep-wake-map-seq
  "액션 타임(분)의 시퀀스를 입력받아 연속된 두 개씩 묶어 자는 시간과 일어난 시간의 해시 맵의 시퀀스로 반환
  Input: [6 25 38 51 ...]
  Output: [{:sleep 6
            :wake 25},
           {:sleep 38
            :wake 51}, ...]"
  [actions]
  (->> actions
       (partition 2)
       (map #(zipmap [:sleep :wake] %))))

(defn separate-log-to-guards-actions
  "파싱된 로그들을 입력받아 가드마다의 액션 타임(분)을 집계
  포맷: {:guard-id guard-id
        :actions (asleep awake asleep awake ...)}
  ex) [{:guard-id 877
        :actions [{:sleep 6
                   :wake 25},
                  {:sleep 38
                   :wake 51}, ...]},
       {:guard-id 1021
        :actions [{:sleep 15
                   :wake 34},
                  {:sleep 37
                   :wake 44}, ...]}, ...]"
  [logs]
  (->> logs
       (partition-by begins-shift-log?)
       (partition 2)
       (map (fn [[begins-shift-logs actions]]
              {(get-last-guard-id begins-shift-logs) (mapv :minute actions)}))
       (apply merge-with concat)
       (map (fn [[guard-id actions]]
              {:guard-id guard-id
               :actions (actions->asleep-wake-map-seq actions)}))))

(comment
  (actions->asleep-wake-map-seq [2 6 10 44])
  (separate-log-to-guards-actions input))

(defn get-total-sleep-times [{:keys [actions]}]
  (->> actions
       (map #(- (:wake %) (:sleep %)))
       (reduce +)))

(defn get-most-sleep-minute-portion
  "가드 id와 액션들을 입력받아 가장 많이 잤던 분을 찾고, 빈도수와 함께 리턴
  ex) {:guard-id 877
       :minute 30
       :freq 5}"
  [{:keys [guard-id actions]}]
  (->> actions
       (map #(range (:sleep %) (:wake %)))
       (reduce concat)
       frequencies
       (apply max-key val)
       (#(hash-map :guard-id guard-id
                   :minute (key %)
                   :freq (val %)))))

; Part 1

(defn find-most-asleep-guard-actions [guards-actions]
  (->> guards-actions
       (apply max-key get-total-sleep-times)))

(comment
  (->> input
       separate-log-to-guards-actions
       find-most-asleep-guard-actions
       get-most-sleep-minute-portion
       ((juxt :guard-id :minute))
       (apply *)))

; Part 2
(defn find-most-frequently-sleep-same-minute [guard-id->action-minutes]
  (->> guard-id->action-minutes
       (map get-most-sleep-minute-portion)
       (apply max-key :freq)))

(comment
  (->> input
       separate-log-to-guards-actions
       find-most-frequently-sleep-same-minute
       ((juxt :guard-id :minute))
       (apply *)))
