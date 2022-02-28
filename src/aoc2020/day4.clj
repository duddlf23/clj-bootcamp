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
  (let [blank? #(= % "")
        blank-lines-partition? #(blank? (first %))]
    (->> logs
         (partition-by blank?)
         (remove blank-lines-partition?))))

(def int-fields #{"byr" "iyr" "eyr"})

(defn field->map [[field-name field-value]]
  (let [parsed-value (if (int-fields field-name)
                       (parse-int-safely field-value)
                       field-value)]
    (hash-map (keyword field-name) parsed-value)))

(defn parse-one-field
  "하나의 필드 정보를 파싱함
  ex) hcl:#7d3b0c -> {:hcl #7d3b0c}
      byr:1976 -> {:byr 1976}"
  [field]
  (->> (re-find #"(.+):(.+)" field)
       (drop 1)
       field->map))

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

(defn parse-passports [passports]
  (map parse-one-passport passports))

; Part 1

(def required-fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

(defn has-all-required-fields? [fields]
  (set/superset? fields required-fields))

(defn valid-passport? [passport]
  (-> (keys passport)
      set
      has-all-required-fields?))

(defn get-valid-passports [parsed-passports]
    (filter valid-passport? parsed-passports))

(comment
  (parse-one-field "hcl:#7d3b0c")
  (parse-one-passport `("cid:325" "byr:2007 eyr:1933 hgt:188in" "pid:713080083 ecl:#d624ca iyr:2030 hcl:z"))
  (-> input-file
      file/read-file
      split-logs
      parse-passports
      get-valid-passports
      count))

; Part 2
(defn valid-hgt? [hgt]
  (->> (re-find #"^(\d+)(cm|in)$" hgt)
       ((fn [[_ num-str unit]]
          (let [num (parse-int-safely num-str)]
            (case unit
              "cm" (<= 150 num 193)
              "in" (<= 59 num 76)
              false))))))

(s/def ::byr (s/int-in 1920 2003))
(s/def ::iyr (s/int-in 2010 2021))
(s/def ::eyr (s/int-in 2020 2031))
(s/def ::hgt valid-hgt?)
(s/def ::hcl #(re-matches #"^#[0-9a-f]{6}$" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"^[0-9]{9}$" %))
(s/def ::cid any?)

(s/def ::passport (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt-un [::cid]))

(defn get-valid-passports-with-spec [parsed-passports]
         (filter #(s/valid? ::passport %) parsed-passports))

(comment
  (-> input-file
      file/read-file
      split-logs
      parse-passports
      get-valid-passports-with-spec
      count))


; (def optional-fields #{:cid}))

;(defn has-limited-optional-fields? [fields]
;  (-> (set/difference fields required-fields)
;      (set/subset? optional-fields)))
;
;(defn has-valid-fields? [fields]
;  (->> fields
;       ((juxt has-all-required-fields? has-limited-optional-fields?))
;       (every? true?)))