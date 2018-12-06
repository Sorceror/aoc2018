(ns aoc2018.day06
  (require [aoc2018.utils :as u]
           [clojure.string :as s]))

; --- Day 6: Chronal Coordinates ---
; Using only the Manhattan distance, determine the area around each coordinate by counting the number of integer X,Y locations that are closest to that coordinate (and aren't tied in distance to any other coordinate).
;
; Your goal is to find the size of the largest area that isn't infinite.

(def test-input
  ["1, 1" "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])
(def input
      (u/resource->strings "day06.txt"))

(defn input->position-seq [input]
  (->>
      input
      (map #(s/split %1 #"\, "))
      (map #(map read-string %1))
      (map #(into [] %1))))

(defn pos-seq->id-map [pos-seq]
  (->>
      pos-seq
      (map-indexed (fn [idx p] [idx p]))
      (into (hash-map))))

(defn manhattan-distance [[p1x p1y] [p2x p2y]]
  (+ (Math/abs ^Integer (- p2x p1x)) (Math/abs ^Integer (- p2y p1y))))

(defn calculate-closest-id [p id-map]
  (let [distances (map (fn [[k k-pos]] [k (manhattan-distance p k-pos)]) id-map)
        min-val (apply min (map second distances))
        min-ids (filter #(= min-val (second %)) distances)]
    (if (= 1 (count min-ids))
        (first (first min-ids))
        \.)))

(defn calculate-bbox [input-seq]
  (reduce
    (fn [[[min-x min-y] [max-x max-y]] [p-x p-y]]
      [[(min min-x p-x) (min min-y p-y)] [(max max-x p-x) (max max-y p-y)]])
    [[(Integer/MAX_VALUE) (Integer/MAX_VALUE)] [0 0]] input-seq))

(defn calculate-board-ids [input-seq]
  (let [[[min-x min-y] [max-x max-y]] (calculate-bbox input-seq)
        points (for [y (range min-y (inc max-y)) x (range min-x (inc max-x))] [x y])
        id-map (pos-seq->id-map input-seq)]
    (map #(calculate-closest-id %1 id-map) points)))

(defn evaluate-biggest-non-infinite-area [input-seq]
  (let [board (calculate-board-ids input-seq)
        [[min-x _] [max-x _]] (calculate-bbox input-seq)
        bbox-w (inc (- max-x min-x))
        top (take bbox-w board)
        bottom (take-last bbox-w board)
        left-side (take-nth bbox-w board)
        right-side (reverse (take-nth bbox-w (reverse board)))
        border-ids (filter #(not= % \.) (distinct (concat top bottom left-side right-side)))
        filtered-board (replace (zipmap border-ids (repeat \∞)) board)
        counts (frequencies filtered-board)
        areas (dissoc counts \. \∞)]
    (apply max-key second areas)))

(comment
  (let [[id size] (evaluate-biggest-non-infinite-area (input->position-seq input))]
    (println "Biggest non-infinite area has size" size "and id" id)))

; --- Part Two ---
; On the other hand, if the coordinates are safe, maybe the best you can do is try to find a region near as many coordinates as possible.
;
;What is the size of the region containing all locations which have a total distance to all given coordinates of less than 10000?

(defn calculate-all-distances [p id-map]
  (->>
      (map (fn [[_ k-pos]] (manhattan-distance p k-pos)) id-map)
      (reduce +)))

(defn calculate-board-distances [threshold input-seq]
  (let [[[min-x min-y] [max-x max-y]] (calculate-bbox input-seq)
        points (for [y (range min-y (inc max-y)) x (range min-x (inc max-x))] [x y])
        id-map (pos-seq->id-map input-seq)
        board-dist (map #(calculate-all-distances %1 id-map) points)
        filtered-board (map #(if (< %1 threshold) %1 \.) board-dist)]
    filtered-board))

(defn area-size-with-distance-to-all-threshold [threshold input-seq]
  (->>
      input-seq
      (calculate-board-distances threshold)
      (filter #(not= \. %))
      (count)))


(comment
  "This solution expects that the area is connected and not separated in multiple components"
  (println "Size of area where every point has smaller distance to other points than 10000 is"
           (area-size-with-distance-to-all-threshold 10000 (input->position-seq input))))
; Size of area where every point has smaller distance to other points than 10000 is 42535