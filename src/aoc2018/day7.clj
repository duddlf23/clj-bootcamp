(ns aoc2018.day7
  "https://adventofcode.com/2018/day/4"
  (:require [util.file :as file]
            [clojure.string :as string]))


(defn line->dependency
  "라인 하나를 파싱해 디펜던시 정보를 구함 만약 A를 하기 위해 B가 반드시 끝나야 한다면
  {:required B, :step A} 가 리턴된다."
  [line]
  (->> line
       (re-find #"^Step ([A-Z]{1}) must be finished before step ([A-Z]{1}) can begin.$")
       ((fn [[_ required step]]
          {:required required
           :step step}))))

(def input-dependencies (->> (file/read-file "aoc2018/day7_in.txt")
                             (map line->dependency)))

(defn remove-done-dependencies
  "스텝들이 끝났을 때 그 스텝들이 required인 디펜던시들을 모두 제거한다."
  [dependencies done-steps]
  (remove (comp (set done-steps) :required) dependencies))

(defn find-executable-step
  "디펜던시가 모두 끝난 스텝을 하나 찾는데 동시에 가능한 스텝이 여러개가 있을 경우 알파벳 순서로 가장 빠른 스텝부터 고름"
  [dependencies steps]
  (let [dependent-steps (set (map :step dependencies))]
    (->> steps
         (remove dependent-steps)
         sort
         first)))

; Part 1
(defn complete-executable-step
  "현재까지 남은 디펜던시들을 이용해 실행 가능한 스텝을 찾아 만약 존재한다면 그 스텝을 완료했을 때를 기준으로 데이터를 업데이트한다.
  데이터 포맷 예시: {:remain-steps #{A D F}
                 :ordered-done-steps [B C]
                 :dependencies ({:required A, :step D},
                                {:required A, :step F})}"
  [{:keys [remain-steps ordered-done-steps dependencies] :as prev-steps-status}]
  (let [executable-step (find-executable-step dependencies remain-steps)]
    (if-not executable-step
      prev-steps-status
      (hash-map :remain-steps (disj remain-steps executable-step)
                :ordered-done-steps (conj ordered-done-steps executable-step)
                :dependencies (remove-done-dependencies dependencies [executable-step])))))

(defn get-complete-order [dependencies all-steps]
  (->> (iterate complete-executable-step {:remain-steps all-steps
                                          :ordered-done-steps []
                                          :dependencies dependencies})
       (drop-while (comp seq :remain-steps))
       first
       :ordered-done-steps))

(comment
  (line->dependency "Step U must be finished before step A can begin.")

  (let [all-steps (->> input-dependencies
                       (mapcat (juxt :required :step))
                       set)]
    (->> (get-complete-order input-dependencies all-steps)
         (apply str))))


; Part 2
(def step-ids "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn required-time [step]
  (+ 61 (string/index-of step-ids step)))

(defn add-steps-to-workers
  "워커에 스텝들을 추가한다. 만약 새로 추가될 스텝 A가 60초에 추가된다면 121초에 완료되기 때문에 다음과 같은 데이터 형식으로 추가된다.
  {:step A, :completed-time 121}"
  [workers steps current-time]
  (->> steps
       (map #(hash-map :step %
                       :completed-time (+ current-time (required-time %))))
       (into workers)))

(defn find-completed-steps
  "현재 시각이 스텝이 완료되는 시간보다 크다면 그 스텝은 완료됐다는 것이다.
  워커에 등록된 스텝들 중 완료된 스텝들을 찾아 리턴한다."
  [workers current-time]
  (->> workers
       (filter #(<= (:completed-time %) current-time))
       (map :step)))

(defn remove-steps-from-workers
  "스텝들을 입력받아 워커 목록에서 해당 스텝들을 지운다."
  [workers steps]
  (remove (comp (set steps) :step) workers))

(defn find-executable-n-steps
  "디펜던시가 모두 끝난 스텝을 n개 찾는데 현재 실행 중인 스텝은 배제한다. 동시에 가능한 스텝이 여러개가 있을 경우 알파벳 순서로 가장 빠른 스텝부터 고름"
  [dependencies steps in-progress-steps n]
  (let [dependent-steps (set (map :step dependencies))]
    (->> steps
         (remove dependent-steps)
         (remove (set in-progress-steps))
         sort
         (take n))))

(defn update-steps-status
  "1초 전의 워커와 스텝들의 상태를 입력받아 현재 시간에서 완료되는 스텝들과 새로 워커에 추가될 스텝들을 구해 상태를 업데이트한다.
  데이터 포맷 예시: Input: {:remain-steps #{A D F}
                        :workers ({:step A :completed-time 61})
                        :dependencies ({:required A, :step D},
                                       {:required A, :step F})
                        :elapsed-time 61}
                Output: {:remain-steps #{D F}
                         :workers ({:step D :completed-time 125},
                                   {:step F :completed-time 127})
                         :dependencies ()
                         :elapsed-time 62}"
  [{:keys [remain-steps workers dependencies elapsed-time]}]
  (let [in-progress-steps (map :step workers)
        completed-steps (find-completed-steps workers elapsed-time)
        idle-worker-count (- 5 (- (count workers) (count completed-steps)))
        updated-remain-steps (apply disj remain-steps completed-steps)
        updated-dependencies (remove-done-dependencies dependencies completed-steps)
        executable-steps (find-executable-n-steps updated-dependencies updated-remain-steps
                                                  in-progress-steps idle-worker-count)]
    (hash-map :remain-steps updated-remain-steps
              :workers (-> workers
                           (remove-steps-from-workers completed-steps)
                           (add-steps-to-workers executable-steps elapsed-time))
              :dependencies updated-dependencies
              :elapsed-time (inc elapsed-time))))

(defn get-all-steps-completed-time [dependencies all-steps]
  (->> (iterate update-steps-status {:remain-steps all-steps
                                     :workers      []
                                     :dependencies dependencies
                                     :elapsed-time 0})
       (take-while (comp seq :remain-steps))
       last
       :elapsed-time))

(comment
  (required-time "X")
  (find-completed-steps [{:step "A", :completed-time 61}
                         {:step "B", :completed-time 62}], 61)

  (get-all-steps-completed-time [{:required "A" :step "B"}] #{"A" "B"})

  (let [all-steps (->> input-dependencies
                       (mapcat (juxt :required :step))
                       set)]
    (get-all-steps-completed-time input-dependencies all-steps)))
