(ns aoc2020.day8
  "https://adventofcode.com/2020/day/8"
  (:require [util.file :as file]
            [clojure.edn :as edn]
            [clojure.string :as string]))

(defn line->operation [line]
  (let [[op arg] (string/split line #" ")]
    {:op (keyword op)
     :arg (edn/read-string arg)}))

(def input
  "랜덤 엑세스를 위해 벡터로 사용"
  (->> (file/read-file "aoc2020/day8_in.txt")
       (mapv line->operation)))

(defn nop
  "nop 연산을 수행해 스테이트를 업데이트한다."
  [{:keys [cursor] :as pre-state} _]
  (assoc pre-state :cursor (inc cursor)))

(defn jmp
  "jmp 연산을 수행해 스테이트를 업데이트한다."
  [{:keys [cursor] :as pre-state} idx-change]
  (assoc pre-state :cursor (+ cursor idx-change)))

(defn acc
  "acc 연산을 수행해 스테이트를 업데이트한다."
  [{:keys [accumulator cursor] :as pre-state} acc-change]
  (assoc pre-state :accumulator (+ accumulator acc-change) :cursor (inc cursor)))

(def op-funcs
  "op 키워드에 따른 실제 업데이트 함수 매핑"
  {:nop nop
   :jmp jmp
   :acc acc})

(defn update-state-after-op
  "실제 하나의 op를 수행한 후 해당 op에 따라 스테이트를 업데이트하고 커서의 히스토리 또한 같이 업데이트"
  [{:keys [cursor cursor-history] :as pre-state} {:keys [op arg]}]
  (-> ((op-funcs op) pre-state arg)
      (assoc :cursor-history (conj cursor-history cursor))))

(defn update-program-status
  "실제 op를 수행하진 않고 program-status(중복된 명령에 도달했는지, 더 이상 실행할 명령이 없는지) 만 업데이트한다."
  [pre-state status]
  (assoc pre-state :program-status status))

(defn execute-next-op
  "이전의 상태를 받아와 커서 정보를 바탕으로 중복된 명령을 실행했는지,
  프로그램이 끝났는지 검사하거나 다음 오퍼레이션을 실행해 스테이트를 업데이트한다."
  [{:keys [instructions cursor cursor-history] :as pre-state}]
  (let [op (nth instructions cursor nil)]
    (cond
      (cursor-history cursor) (update-program-status pre-state :duplicate-run)
      (nil? op) (update-program-status pre-state :termination)
      :else (update-state-after-op pre-state op))))

(defn execute-instructions [instructions]
  (let [init-state {:instructions instructions
                    :accumulator 0
                    :cursor 0
                    :cursor-history #{}
                    :program-status :running}
        result (->> (iterate execute-next-op init-state)
                    (filter #(-> (:program-status %)
                                 (not= :running)))
                    first)]
    (select-keys result [:accumulator :program-status])))

(comment
  (line->operation "nop +0")
  (execute-instructions input))

; Part 2

; set을 함수로 썼기에 엄밀히 말하면 리턴이 boolean 값은 아니다. 그런데도 함수 이름에 ?을 붙여 써도 되나
(defn nop-jmp-op? [{op :op}]
  (#{:jmp :nop} op))

(defn get-nop-jmp-cursors
  "keep-indexed를 활용해 instruction들 중 nop이나 jmp op의 커서들만 찾는다."
  [instructions]
  (keep-indexed #(when (nop-jmp-op? %2) %1) instructions))

(def change-op
  "nop이라면 jmp로, jmp라면 nop으로 바꾼다."
  {:nop :jmp
   :jmp :nop})

(defn change-op-at-cursor
  "특정 커서의 op를 nop <-> jmp 로 바꾼다."
  [instructions cursor]
  (let [{:keys [op arg]} (nth instructions cursor)]
    (assoc instructions cursor {:op (change-op op)
                                :arg arg})))

(comment
  (->> (get-nop-jmp-cursors input)
       (map (partial change-op-at-cursor input))
       (map execute-instructions)
       (filter #(-> (:program-status %)
                    (= :termination)))
       first))
