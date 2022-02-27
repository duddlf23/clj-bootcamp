(ns aoc2018.day4
  "https://adventofcode.com/2018/day/4"
  (:require [util.file :as file]
            [clojure.edn :as edn])
  (:import (java.time Duration LocalDateTime)
           (java.time.format DateTimeFormatter)))

(def log-date-time-format (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))

(defn parse-to-local-date-time [date-time-str]
  (LocalDateTime/parse date-time-str log-date-time-format))

(defn get-minutes [local-date-time]
  (.getMinute local-date-time))

(defn get-minute-part-after-interval [start-date-time interval-minutes]
  (->> (.plusMinutes start-date-time interval-minutes)
       get-minutes))

(defn get-duration-as-minutes [start-date-time end-date-time]
  (->> (Duration/between start-date-time end-date-time)
       .toMinutes))

(defn get-duration-minutes-seq [start-date-time end-date-time]
  (->> (get-duration-as-minutes start-date-time end-date-time)
       range
       (map (partial get-minute-part-after-interval start-date-time))))

(defn get-duration-minute-frequencies [start-date-time end-date-time]
  (->> (get-duration-minutes-seq start-date-time end-date-time)
       frequencies))

{:year  2020
 :month 4}



(comment
  (parse-to-local-date-time "1518-05-11 00:58")
  (def mock-date-time (parse-to-local-date-time "1518-05-11 00:58"))
  (def mock-date-time2 (parse-to-local-date-time "1518-05-11 03:58"))
  (.toMinutes (Duration/between  (parse-to-local-date-time "1518-05-11 00:58")  (parse-to-local-date-time "1518-05-11 02:59")))
  (get-duration-as-minutes (parse-to-local-date-time "1518-05-11 00:58")
                           (parse-to-local-date-time "1518-05-11 03:58"))
  (get-minute-part-after-interval mock-date-time 3)
  (get-duration-minutes-seq mock-date-time mock-date-time2)
  (get-duration-minute-frequencies mock-date-time mock-date-time2)
  (.plusMinutes (parse-to-local-date-time "1518-05-11 03:58") 1)
  (->> (parse-to-local-date-time "1518-05-11 00:58")
       get-minutes))

(def input-file "aoc2018/day4_in.txt")
(def input (file/read-file input-file))

(defn parse-one-line-to-hash-map [line]
  (->> line
       (re-find #"\[(.+)\][^0-9]*(\d+)?")
       (drop 1)
       ((fn [[date-time guard-id]]
          {:local-date-time (parse-to-local-date-time date-time)
           :guard-id (edn/read-string guard-id)}))))

(defn parse-input-log [file-name]
  (->> (file/read-file file-name)
       (map parse-one-line-to-hash-map)))

(defn get-date-time-sorted-logs [logs]
  (sort-by :local-date-time logs))

(defn is-begins-shift-log? [log]
  (->> log
       :guard-id
       some?))
for
(defn separate-begin-log [date-time-sorted-logs]
  (->> date-time-sorted-logs
       (partition-by is-begins-shift-log?)
       (apply hash-map)))

(defn get-asleep-minutes-seq [action-logs]
  (->> action-logs
       (partition 2)
       (map (fn [[{fall-asleep-time :local-date-time}
                  {wake-up-time :local-date-time}]]
              (get-duration-minutes-seq fall-asleep-time wake-up-time)))
       (reduce concat)))

(defn get-total-asleep-times [action-logs]
  (->> action-logs
       (partition 2)
       (map (fn [[{fall-asleep-time :local-date-time}
                  {wake-up-time :local-date-time}]]
              (get-duration-as-minutes fall-asleep-time wake-up-time)))
       (reduce +)))

(defn aggregate-guard-asleep-times [separate-begin-log]
  (->> separate-begin-log
       (map (fn [[[begin-log] action-logs]]
              {(:guard-id begin-log) (get-total-asleep-times action-logs)}))
       (apply merge-with +)))


(defn aggregate-guard-asleep-minutes [separate-begin-log]
  (->> separate-begin-log
       (map (fn [[[begin-log] action-logs]]
              {(:guard-id begin-log) (get-asleep-minutes-seq action-logs)}))
       (apply merge-with concat)))


(defn find-most-asleep-guard-id [guard-asleep-times]
  (->> guard-asleep-times
       (apply max-key val)
       key))

(defn get-asleep-minutes-frequencies-certain-guard [guard-id guard-asleep-minutes]
  (->> (get guard-asleep-minutes guard-id)
       frequencies))



(comment
  (parse-one-line-to-hash-map "[1518-05-11 00:58] wakes up")
  (parse-one-line-to-hash-map "[1518-08-17 00:01] Guard #1021 begins shift")
  (def parsed-input (parse-input-log input-file))
  (def sorted-logs (get-date-time-sorted-logs parsed-input))
  (separate-begin-log sorted-logs)
  (aggregate-guard-asleep-times (separate-begin-log sorted-logs))
  (def most-asleap-guard-id (->> sorted-logs
                                 separate-begin-log
                                 aggregate-guard-asleep-times
                                 find-most-asleep-guard-id))
  (frequencies (get (->> sorted-logs
                         separate-begin-log
                         aggregate-guard-asleep-minutes) 571))
  (->> sorted-logs
       separate-begin-log
       aggregate-guard-asleep-minutes
       (get-asleep-minutes-frequencies-certain-guard most-asleap-guard-id)
       (apply max-key val)
       key))
