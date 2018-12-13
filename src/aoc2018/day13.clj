(ns aoc2018.day13
  (require [aoc2018.utils :as u]))

;--- Day 13: Mine Cart Madness ---
; You map out the tracks (your puzzle input) and see where you can help.
; Tracks consist of straight paths (| and -), curves (/ and \), and intersections (+). Curves connect exactly two perpendicular pieces of track)
; Several carts are also on the tracks. Carts always face either up (^), down (v), left (<), or right (>). (On your initial map, the track under each cart is a straight path matching the direction the cart is facing.)
; Each time a cart has the option to turn (by arriving at any intersection), it turns left the first time, goes straight the second time, turns right the third time, and then repeats those directions starting again with left the fourth time, straight the fifth time, and so on. This process is independent of the particular intersection at which the cart has arrived - that is, the cart has no per-intersection memory.
; Carts all move at the same speed; they take turns moving a single step at a time. They do this based on their current location: carts on the top row move first (acting from left to right), then carts on the second row move (again from left to right), then carts on the third row, and so on. Once each cart has moved one step, the process repeats; each of these loops is called a tick.
; After following their respective paths for a while, the carts eventually crash. To help prevent crashes, you'd like to know the location of the first crash. Locations are given in X,Y coordinates, where the furthest left column is X=0 and the furthest top row is Y=0

(def input
  (u/resource->strings "day13.txt" false))

(defn parse-cars-tracks [input]
  (->> input
      (map-indexed (fn [y row] (map-indexed (fn [x c] [[x y] c]) row)))
      (reduce into (vector))
      (filter (fn [[_ c]] (not= c \space)))
      (reduce
        (fn [[cars tracks][[x y] c]]
          (cond
            (= c \^) [(conj cars [[x y] :up :right]) (conj tracks [[x y] \|])]
            (= c \v) [(conj cars [[x y] :down :right]) (conj tracks [[x y] \|])]
            (= c \>) [(conj cars [[x y] :right :right]) (conj tracks [[x y] \-])]
            (= c \<) [(conj cars [[x y] :left :right]) (conj tracks [[x y] \-])]
            :else    [cars (conj tracks [[x y] c])]))
        [[] []])
      ((fn [[cars tracks]] [cars (into {} tracks)]))))

(defn get-track [pos tracks]
  (let [track (get tracks pos)]
    (if (nil? track)
        (println "Cannot find track for" pos)
        track)))

(defn move [[x y] dir]
  (cond
    (= dir :up) [x (dec y)]
    (= dir :down) [x (inc y)]
    (= dir :left) [(dec x) y]
    (= dir :right) [(inc x) y]))

(defn left<->right [dir]
  (cond
    (= dir :left) :left
    (= dir :right) :right
    :else (do
            (println "Cannot move on left<-> right with direction" dir)
            dir)))

(defn up<->down [dir]
  (cond
    (= dir :up) :up
    (= dir :down) :down
    :else (do
            (println "Cannot move on up<->down with direction" dir)
            dir)))

; /---     |
; |     ---/
(defn corner-forward [dir]
  (cond
    (= dir :up) :right
    (= dir :left) :down
    (= dir :down) :left
    (= dir :right) :up))

; |     ---\
; \---     |
(defn corner-backward [dir]
  (cond
    (= dir :down) :right
    (= dir :left) :up
    (= dir :up) :left
    (= dir :right) :down))

(defn rotate-direction [dir rotation]
  (cond
    (= rotation :forward) dir
    (= rotation :right) (cond
                          (= dir :up) :right
                          (= dir :right) :down
                          (= dir :down) :left
                          (= dir :left) :up)
    (= rotation :left) (cond
                         (= dir :up) :left
                         (= dir :left) :down
                         (= dir :down) :right
                         (= dir :right) :up)
    :else (do
            (println "Unsupported rotation" rotation "with direction" dir)
            dir)))

(defn intersection-rotation [last-intersection]
  (cond
    (= last-intersection :right) :left
    (= last-intersection :left) :forward
    (= last-intersection :forward) :right))

(defn change-direction [pos dir last-intersection tracks]
  (let [track (get-track pos tracks)]
    (cond
      (= track \-) [(left<->right dir) last-intersection]
      (= track \|) [(up<->down dir) last-intersection]
      (= track \/) [(corner-forward dir) last-intersection]
      (= track \\) [(corner-backward dir) last-intersection]
      (= track \+)
      (let [rot (intersection-rotation last-intersection)]
        [(rotate-direction dir rot) rot])
      :else (do
              (println "Unsupported track type" track "on pos" pos)
              [dir last-intersection]))))

(defn first-collision-coordinates [cars]
  (->> cars
       (map first)
       (frequencies)
       (filter (fn [[_ q]] (> q 1)))
       (first)
       (first)))

(defn tick [cars tracks]
  (map
    (fn [[pos dir last-intersection]]
      (let [new-pos (move pos dir)
            [new-dir new-last] (change-direction new-pos dir last-intersection tracks)]
        [new-pos new-dir new-last]))
    cars))

(defn find-first-collision [input]
  (let [[cars tracks] (parse-cars-tracks input)]
    (loop [curr-cars cars]
      (let [next-cars (tick curr-cars tracks)
            collision (first-collision-coordinates next-cars)]
        (if (some? collision)
          collision
          (recur next-cars))))))

(comment
  (let [[x y] (find-first-collision input)]
    (println "First collision occurs at" (format "%d,%d" x y))))
; First collision occurs at 32,99

; --- Part Two ---
; There isn't much you can do to prevent crashes in this ridiculous system. However, by predicting the crashes, the Elves know where to be in advance and instantly remove the two crashing carts the moment any crash occurs.
; They can proceed like this for a while, but eventually, they're going to run out of carts. It could be useful to figure out where the last cart that hasn't crashed will end up.
; What is the location of the last cart at the end of the first tick where it is the only cart left?

; (╯°□°）╯︵ ┻━┻) so much state in state, that is disgusting

(defn all-collision-coordinates [cars]
  (->> cars
       (map first)
       (frequencies)
       (filter (fn [[_ q]] (> q 1)))
       (map first)))

(def cars-comparator
  (comparator (fn [[x1 y1] [x2 y2]] (if (= y2 y1) (> x2 x1) (> y2 y1)))))

(defn remove-collisions [cars]
  (let [collisions (all-collision-coordinates cars)]
    (apply vector (filter (fn [[pos _ _]] (not-any? #(= pos %) collisions)) cars))))

(defn tick-2 [cars tracks]
  (let [sorted-cars (apply vector (sort-by first cars-comparator cars))]
    (loop [updated-cars sorted-cars
           idx 0]
      (if (or (>= idx (count updated-cars)) (= (count updated-cars) 1))
          updated-cars
          ; OMG so much state :(
          (let [[pos dir last-intersection] (nth updated-cars idx)
                new-pos (move pos dir)
                [new-dir new-last] (change-direction new-pos dir last-intersection tracks)
                new-cars (assoc updated-cars idx [new-pos new-dir new-last])
                collision-free (remove-collisions new-cars)
                new-idx (if (= (count collision-free) (count new-cars)) (inc idx) idx)]
            (recur collision-free new-idx))))))

(defn run-until-last-cart [input]
  (let [[cars tracks] (parse-cars-tracks input)]
    (loop [curr-cars cars]
      (let [next-cars (tick-2 curr-cars tracks)]
        (if (= (count next-cars) 1)
            next-cars
            (recur next-cars))))))

(comment
  (let [[x y] (first (first (run-until-last-cart input)))]
    (println "Last cart position is" (format "%d,%d" x y))))
; Last cart position is 56,31