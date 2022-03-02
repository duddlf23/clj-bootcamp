(ns aoc2018.day6
  "https://adventofcode.com/2018/day/6"
  (:require [util.file :as file]
            [clojure.edn :as edn]))

(defn line->coordinate
  "ex) 154, 656 -> {:x 154, :y 656}"
  [line]
  (->> line
       (re-find #"(\d+), (\d+)")
       (drop 1)
       (map edn/read-string)
       (zipmap [:x :y])))

(defn merge-with-target-id [target-id {:keys [x y]}]
  {:target-id target-id
   :x x
   :y y})

(def input-targets
  "입력 좌표들을 파싱하고, 0부터 차례대로 증가된 인덱스를 target-id로 합침
  ex) Input: 350, 354
             238, 298 ...
      Output: [{:target-id 0, :x 350, :y 353},
               {:target-id 1, :x 238, :y 298}, ...]"
  (->> (file/read-file "aoc2018/day6_in.txt")
       (map line->coordinate)
       (map merge-with-target-id (range))))

(defn get-border-coordinates [points]
  (let [xs (map :x points)
        ys (map :y points)]
    {:min-x (apply min xs)
     :max-x (apply max xs)
     :min-y (apply min ys)
     :max-y (apply max ys)}))

(defn get-points-in-bounded-area [{:keys [min-x max-x min-y max-y]}]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))]
    {:x x
     :y y}))

(defn border-coords->point-in-boundary? [{:keys [min-x max-x min-y max-y]}]
  (fn [{:keys [x y]}]
    (or (= min-x x)
        (= max-x x)
        (= min-y y)
        (= max-y y))))

(defn abs [n] (max n (- n)))

(defn get-distance [{x1 :x y1 :y}
                    {x2 :x y2 :y}]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn get-distances-to-targets
  "타겟 포인트들과 임의의 포인트를 입력받고 타겟 포인트들과의 거리를 구한다.
  ex) Input
        target: [{:target-id 0, :x 350, :y 353},
                 {:target-id 1, :x 238, :y 298}, ...]
        point: {:x 355, :y 358}
      Output: [{:target-id 0, :distance 10},
               {:target-id 1, :distance 177}, ...]"
  [targets point]
  (let [target-ids (map :target-id targets)]
    (->> targets
         (map #(get-distance point %))
         (map #(hash-map :target-id %1 :distance %2) target-ids))))

;; Part 1

(defn get-closest-target-ids
  "문제에서 주어진 타겟 포인트들과 임의의 포인트를 입력받아 이 포인트와 가장 가까운 타겟 포인트들을 구한다.
  ex) Output: #{0 4} - target-id 0, 4인 타켓 포인트들과 가장 가까움"
  [targets point]
  (->> (get-distances-to-targets targets point)
       (group-by :distance)
       (apply min-key key)
       val
       (map :target-id)
       set))

(defn closest-target-unique? [{:keys [closest-target-ids]}]
  (= 1 (count closest-target-ids)))

(defn get-finite-areas
  "타겟 포인트들로 인해 생성되는 영역들 중 유한 영역들의 시퀀스를 구한다.
  하나의 영역은 포함된 점들의 시퀀스로 표현했다.
  ex) Output: (({:x 309, :y 274}
                {:x 309, :y 275}, ...),
               ({:x 135, :y 237}
                {:x 136, :y 237}, ...), ...)"
  [targets]
  (let [border-coords (get-border-coordinates targets)
        points-in-bounded-area (get-points-in-bounded-area border-coords)
        point-in-boundary? (border-coords->point-in-boundary? border-coords)
        infinite-area? #(some point-in-boundary? %)]
    (->> points-in-bounded-area
         (map #(get-closest-target-ids targets %))
         (map #(hash-map :point %1 :closest-target-ids %2) points-in-bounded-area)
         (filter closest-target-unique?)
         (group-by :closest-target-ids)
         vals
         (map (partial map :point))
         (remove infinite-area?))))


(comment
  (->> input-targets
       get-finite-areas
       (map count)
       (apply max)))

; Part 2
(defn sum-all-distances-to-targets
  [targets point]
  (->> (get-distances-to-targets targets point)
       (map :distance)
       (reduce +)))

(comment
  (sum-all-distances-to-targets input-targets {:x 355 :y 353})
  (let [border-coords (get-border-coordinates input-targets)
        points-in-bounded-area (get-points-in-bounded-area border-coords)
        point-in-boundary? (border-coords->point-in-boundary? border-coords)
        get-total-distance (partial sum-all-distances-to-targets input-targets)]

    (->> points-in-bounded-area
         (filter point-in-boundary?)
         (map get-total-distance)
         (apply min))
    ; -> 10467
    ; 입력으로 주어진 점들로 이뤄진 경계 영역의 경계선에 있는 점들은 모두 거리의 합이 10000보다 크기 때문에 그 영역 안만 고려한다.

    (->> points-in-bounded-area
         (map get-total-distance)
         (filter #(< % 10000))
         count)))
