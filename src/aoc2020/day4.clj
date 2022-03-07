(ns aoc2020.day4
  "https://adventofcode.com/2020/day/4"
  (:require [util.file :as file]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input-file "aoc2020/day4_in.txt")

(defn parse-int-safely [number-string]
  (try (Integer/parseInt number-string)
       (catch Exception e nil)))

(defn split-logs [logs]
  (->> logs
       (partition-by str/blank?)
       (remove #(some str/blank? %))))

(defn parse-height [hgt]
  (let [[_ value unit] (re-find #"^(\d+)(cm|in)$" hgt)]
    {:unit unit
     :value (parse-int-safely value)}))

(def key->parse-fn
  {:byr parse-int-safely
   :iyr parse-int-safely
   :eyr parse-int-safely
   :hgt parse-height})

(defn parse-one-field
  "하나의 필드 정보를 파싱함
  ex) hcl:#7d3b0c -> {:hcl #7d3b0c}
      byr:1976 -> {:byr 1976}"
  [field]
  (let [[_ k v] (re-find #"(.+):(.+)" field)
        field-keyword (keyword k)
        parse (key->parse-fn field-keyword identity)]
    {field-keyword (parse v)}))

(defn parse-one-passport
  "여러 줄로 나뉘어진 하나의 여권 정보를 해시 맵으로 변환
  ex) Input: ['cid:325'
              'byr:2007 eyr:1933 hgt:188in'
              'pid:713080083 ecl:#d624ca iyr:2030 hcl:z']
      Output: {:cid 325,
               :byr 2007,
               :eyr 1933,
               :hgt '188in',
               :pid 713080083,
               :ecl '#d624ca',
               :iyr 2030,
               :hcl 'z'}"
  [batch-logs]
  (->> batch-logs
       (mapcat #(str/split % #" "))
       (map parse-one-field)
       (apply merge)))

(def input (->> (split-logs (file/read-file input-file))
                (map parse-one-passport)))

(comment
  (parse-one-field "hcl:#7d3b0c")
  (parse-one-passport `("cid:325" "byr:2007 eyr:1933 hgt:188in" "pid:713080083 ecl:#d624ca iyr:2030 hcl:z")))

; Part 1

(def required-fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

(defn has-all-required-fields? [fields]
  (set/superset? fields required-fields))

(defn valid-passport? [passport]
  (-> (keys passport)
      set
      has-all-required-fields?))

(comment
  (->> input
       (filter valid-passport?)
       count))

; Part 2
(defn valid-height? [{:keys [unit value]}]
  (case unit
    "cm" (<= 150 value 193)
    "in" (<= 59 value 76)
    false))

(s/def ::byr (s/int-in 1920 2003))
(s/def ::iyr (s/int-in 2010 2021))
(s/def ::eyr (s/int-in 2020 2031))
(s/def ::hgt valid-height?)
(s/def ::hcl #(re-matches #"^#[0-9a-f]{6}$" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"^[0-9]{9}$" %))
(s/def ::cid any?)

(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-un [::cid]))

(comment
  (-> (filter #(s/valid? ::passport %) input)
      count))