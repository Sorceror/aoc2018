(ns aoc2018.day03
  (require [aoc2018.utils :as u]
           [clojure.set :as set]))

; --- Day 3: No Matter How You Slice It ---
;
; The whole piece of fabric they're working on is a very large square - at least 1000 inches on each side.
;
; Each Elf has made a claim about which area of fabric would be ideal for Santa's suit. All claims have an ID and consist of a single rectangle with edges parallel to the edges of the fabric. Each claim's rectangle is defined as follows:
;
; The number of inches between the left edge of the fabric and the left edge of the rectangle.
; The number of inches between the top edge of the fabric and the top edge of the rectangle.
; The width of the rectangle in inches.
; The height of the rectangle in inches.
; A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall.
;
; The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas. For example, consider the following claims:
;
; #1 @ 1,3: 4x4
; #2 @ 3,1: 4x4
; #3 @ 5,5: 2x2
;
; If the Elves all proceed with their own plans, none of them will have enough fabric. How many square inches of fabric are within two or more claims?

(def input
  (u/resource->strings "day03.txt"))

(defn input->seq [input]
  (->> input
       (map #(re-find #"\#(\d+).@.(\d+)\,(\d+):.(\d+)x(\d+)" %1))
       (reduce
         (fn [m v]
           (conj m [
                    (u/s->i (nth v 1))
                    {:left-top [(u/s->i (nth v 2)) (u/s->i (nth v 3))]
                     :size     [(u/s->i (nth v 4)) (u/s->i (nth v 5))]}]))

         [])))

(defn generate-positions [[x y] [w h]]
  (for [x (range x (+ x w)) y (range y (+ y h))] (vector x y)))

; based on https://gamedev.stackexchange.com/a/62296/8003
(defn is-collision [[[x1 y1] [w1 h1]] [[x2 y2] [w2 h2]]]
  (not
    (or
      (> x2 (+ x1 w1))
      (< (+ x2 w2) x1)
      (> y2 (+ y1 h1))
      (< (+ y2 h2) y1))))

; based on https://stackoverflow.com/a/31022629/600132
(defn eval-overlap [[x1 w1] [x2 w2]]
  [
   (Math/max ^Integer x1 ^Integer x2)
   (-
     ; lowest end point
     (Math/min ^Integer (+ x1 w1) ^Integer (+ x2 w2))
     ; highest start point
     (Math/max ^Integer x1 ^Integer x2))])


(defn collision-coordinates [[[x1 y1] [w1 h1]] [[x2 y2] [w2 h2]]]
  (if (is-collision [[x1 y1] [w1 h1]] [[x2 y2] [w2 h2]])
      (let [[x w] (eval-overlap [x1 w1] [x2 w2])
            [y h] (eval-overlap [y1 h1] [y2 h2])]
        (into #{} (generate-positions [x y] [w h])))
      #{}))

(defn collide-with-others [[_ rect] others]
  (reduce
    (fn [s [_ curr-rect]]
      (set/union s
                 (collision-coordinates [(get rect :left-top) (get rect :size)] [(get curr-rect :left-top) (get curr-rect :size)])))
    #{} others))

(defn evaluate-collisions [ids]
  (loop [current (first ids)
         to-see (rest ids)
         collisions #{}]
    (if (empty? to-see)
        collisions
        (recur (first to-see) (rest to-see) (set/union collisions (collide-with-others current to-see))))))

(defn part1 []
  (->> input
       (input->seq)
       (evaluate-collisions)
       (count)))

(comment
  (println "Overlapping parts of fabric" (part1)))
; Overlapping parts of fabric 116920

; --- Part Two ---
; Amidst the chaos, you notice that exactly one claim doesn't overlap by even a single square inch of fabric with any other claim. If you can somehow draw attention to it, maybe the Elves will be able to make Santa's suit after all!
;
; What is the ID of the only claim that doesn't overlap?
(defn does-not-overlap [ids]
  (loop [current (first ids)
         to-see (rest ids)]
    (if (empty? (collide-with-others current (filter #(not= (first current) (first %1)) ids)))
        (first current)
        (recur (first to-see) (rest to-see)))))

(defn part2 []
  (->> input
       (input->seq)
       (does-not-overlap)))

(comment
  (println "ID of the part that does not overlap with any other parts" (part2)))
; ID of the part that does not overlap with any other parts 382