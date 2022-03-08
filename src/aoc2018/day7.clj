(ns aoc2018.day7
  "https://adventofcode.com/2018/day/4"
  (:require [util.file :as file]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn line->dependency
  "라인 하나를 파싱해 디펜던시 정보를 구함 만약 A를 하기 위해 B가 반드시 끝나야 한다면
  {:required B, :step A} 가 리턴된다."
  [line]
  (let [[_ required step] (re-find #"^Step ([A-Z]{1}) must be finished before step ([A-Z]{1}) can begin.$" line)]
    {:required (keyword required)
     :step (keyword step)}))

(def input-dependencies (->> (file/read-file "aoc2018/day7_in.txt")
                             (map line->dependency)))

(def all-steps-set (->> "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                        (map #(keyword (str %)))
                        set))

;; state의 데이터 포맷 설명
;; {:idle-steps-set 현재까지 실행되거나 완료되지 않은 스텝들의 집합
;;  :accumulated-done-steps 현재까지 완료된 스텝들의 시퀀스
;;  :workers 워커 정보의 시퀀스로 되어 있으며 하나의 워커 정보는 워커에서 실행 중인 스텝과 완료되는 시간이 저장된다.
;;    ex) ({:step A :done-time 134}, {:step B :done-time 100}, ...)
;;  :worker-limit 워커 수의 제한
;;  :step->required-time 스텝을 입력받아 각 스텝이 완료되는데 필요한 시간을 리턴하는 함수
;;  :dependencies 스텝간의 디펜던시 그래프
;;  :elapsed-time 지금까지 경과된 시간
;;  :end? 모든 스텝이 완료됐는지 여부}

(defn newify-done-steps
  "현재 시간에 새롭게 끝난 스텝들을 구하고, 워커와 디펜던시에서 해당 스텝들을 제거한다."
  [{:keys [workers dependencies elapsed-time] :as pre-state}]
  (let [done-steps-set (->> workers
                            (filter #(<= (:done-time %) elapsed-time))
                            (map :step)
                            set)
        active-workers (remove #(done-steps-set (:step %)) workers)
        active-dependencies (remove #(done-steps-set (:required %)) dependencies)]
    (-> (update pre-state :accumulated-done-steps into done-steps-set)
        (assoc :workers active-workers)
        (assoc :dependencies active-dependencies))))

(defn find-next-idle-steps
  "워커에 빈 슬롯만큼 새로 실행할 수 있는 스텝들을 찾는다. 이 때 스텝은 idle 상태이고, 디펜던시가 모두 끝난 상태여야 한다.
  동시에 가능한 스텝이 여러개가 있을 경우 알파벳 순서로 가장 빠른 스텝부터 고른다."
  [{:keys [idle-steps-set workers worker-limit dependencies]}]
  (let [idle-workers-num (- worker-limit (count workers))
        dependent-steps (set (map :step dependencies))]
    (->> idle-steps-set
         (remove dependent-steps)
         sort
         (take idle-workers-num))))

(defn start-next-steps
  "새로 실행할 수 있는 스텝들을 찾고, 각자 지금 시작했을 때 끝나는 시간들을 같이 묶는다. 그 후 워커 목록에 추가하고, idle step 목록에서 제거한다.
  ex) 만약 스텝 A가 60초에 시작한다면 121초에 완료되기 때문에 다음과 같은 데이터 형식으로 워커에 추가되고, 스텝 A는 idle step에서 제거된다.
      {:step A, :done-time 121}"
  [{:keys [step->required-time elapsed-time] :as pre-state}]
  (let [next-steps (find-next-idle-steps pre-state)
        start-step (fn [step]
                     {:step step
                      :done-time (+ elapsed-time (step->required-time step))})]
    (-> (update pre-state :workers into (map start-step next-steps))
        (update :idle-steps-set set/difference (set next-steps)))))

(defn check-end
  "모든 스텝들이 완료됐는지 검사해 상태를 업데이트한다."
  [{:keys [idle-steps-set workers] :as pre-state}]
  (assoc pre-state :end? (and (empty? idle-steps-set)
                              (empty? workers))))

(defn update-steps-state
  "1초 전의 워커와 스텝들의 상태를 입력받아 현재 시간에서 완료되는 스텝들과 새로 워커에 추가될 스텝들을 구해 상태를 업데이트한다.
  아래 예시는 워커에서 돌고 있던 스텝 A가 완료되고 새로 D, F를 워커에 등록하는 프로세스이다.
  데이터 포맷 예시: Input: {:idle-steps #{D F}
                        :accumulated-done-steps []
                        :workers ({:step A :done-time 61})
                        :dependencies ({:required A, :step D},
                                       {:required A, :step F})
                        :elapsed-time 60
                        ...}
                Output: {:idle-steps #{}
                         :accumulated-done-steps [A]
                         :workers ({:step D :done-time 125},
                                   {:step F :done-time 127})
                         :dependencies ()
                         :elapsed-time 61
                         ...}"
  [pre-state]
  (-> (update pre-state :elapsed-time inc)
      newify-done-steps
      start-next-steps
      check-end))

(defn execute-all-steps [{:keys [dependencies all-steps-set worker-limit step->required-time]}]
  (let [init-state {:idle-steps-set         all-steps-set
                    :accumulated-done-steps []
                    :workers                []
                    :worker-limit           worker-limit
                    :step->required-time    step->required-time
                    :dependencies           dependencies
                    :elapsed-time           -1
                    :end?                   false}
        result (->> init-state
                    (iterate update-steps-state)
                    (filter :end?)
                    first)]
    (-> result
        (select-keys [:accumulated-done-steps :elapsed-time]))))

(def step-ids "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn step->required-time-part2 [step]
  (let [duration-offset 61]
    (+ duration-offset (string/index-of step-ids (name step)))))

(comment
  ;; Part 1
  (let [config {:dependencies input-dependencies
                :all-steps-set all-steps-set
                :worker-limit 1
                :step->required-time (constantly 1)}]
    (->> (execute-all-steps config)
         :accumulated-done-steps
         (map name)
         (apply str)))

  ;; Part 2
  (let [config {:dependencies input-dependencies
                :all-steps-set all-steps-set
                :worker-limit 5
                :step->required-time step->required-time-part2}]
    (->> (execute-all-steps config)
         :elapsed-time)))