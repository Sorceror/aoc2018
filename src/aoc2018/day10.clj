(ns aoc2018.day10
  (require [aoc2018.utils :as u]
           [clojure.string :as str]))

; --- Day 10: The Stars Align ---
; You can see these points of light floating in the distance, and record their position in the sky and their velocity, the relative change in position per second (your puzzle input). The coordinates are all given from your perspective; given enough time, those positions and velocities will move the points into a cohesive message!
; What message will eventually appear in the sky?

; --- Part Two ---
; Exactly how many seconds would they have needed to wait for that message to appear?

(def input
  (u/resource->strings "day10.txt"))

(defn parse-input [input]
  (->> input
       (map #(re-find #"position=<\s?(-?\d*),.?\s?(-?\d*)>.velocity=<\s?(-?\d*),.?\s?(-?\d*)>" %))
       (map rest)
       (map #(reduce (fn [v str] (conj v (u/s->i str))) [] %))))

(defn calculate-bounding-box [data]
  (reduce (fn [[min-x min-y max-x max-y _] [x y _ _]]
            (let [bb-min-x (min min-x x)
                  bb-min-y (min min-y y)
                  bb-max-x (max max-x x)
                  bb-max-y (max max-y y)
                  ; *' and other math operations with ' are auto-cast to bigint (avoid int overflow)
                  bb-size (+' (*' (-' max-x min-x) (-' max-x min-x)) (*' (-' max-y y) (-' max-y y)))]
              [bb-min-x bb-min-y bb-max-x bb-max-y bb-size]))
          [Integer/MAX_VALUE Integer/MAX_VALUE Integer/MIN_VALUE Integer/MIN_VALUE 0] data))

(defn print-data [data]
  (let [[min-x min-y max-x max-y _] (calculate-bounding-box data)
        data-normalized (map (fn [[x y _ _]] [(- x min-x) (- y min-y)]) data)
        group-by-rows (group-by (fn [[_ y]] y) data-normalized)]
    (doall
      (for [row (range 0 (inc (- max-y min-y)))]
        (let [row-data (apply vector (repeat (inc (- max-x min-x)) \.))
              filled-row-data (reduce (fn [line [x _]] (assoc line x \#)) row-data (get group-by-rows row))]
          (println filled-row-data))))))

(defn find-text [input]
  (loop [data input
         iteration 0]
    (let [next-step (map (fn [[x y vx vy]] [(+ x vx) (+ y vy) vx vy]) data)
          [_ _ _ _ bb-size] (calculate-bounding-box data)
          [_ _ _ _ next-bb-size] (calculate-bounding-box next-step)]
      (if (<= next-bb-size bb-size)
          (recur next-step (inc iteration))
          [data iteration]))))

(comment
  (let [[data iteration] (find-text (parse-input input))]
    (println "Text output")
    (print-data data)
    (println "Took" iteration "seconds to appear")))
; Text output
; [# # # # # . . . # . . . . # . . # # # # # . . . . . . # # # . . . # # # # . . . # . . . . . . . # # # # # . . . # # # # # #]
; [# . . . . # . . # . . . . # . . # . . . . # . . . . . . # . . . # . . . . # . . # . . . . . . . # . . . . # . . # . . . . .]
; [# . . . . # . . # . . . . # . . # . . . . # . . . . . . # . . . # . . . . . . . # . . . . . . . # . . . . # . . # . . . . .]
; [# . . . . # . . # . . . . # . . # . . . . # . . . . . . # . . . # . . . . . . . # . . . . . . . # . . . . # . . # . . . . .]
; [# # # # # . . . # # # # # # . . # # # # # . . . . . . . # . . . # . . . . . . . # . . . . . . . # # # # # . . . # # # # # .]
; [# . . . . # . . # . . . . # . . # . . . . . . . . . . . # . . . # . . # # # . . # . . . . . . . # . . . . . . . # . . . . .]
; [# . . . . # . . # . . . . # . . # . . . . . . . . . . . # . . . # . . . . # . . # . . . . . . . # . . . . . . . # . . . . .]
; [# . . . . # . . # . . . . # . . # . . . . . . . # . . . # . . . # . . . . # . . # . . . . . . . # . . . . . . . # . . . . .]
; [# . . . . # . . # . . . . # . . # . . . . . . . # . . . # . . . # . . . # # . . # . . . . . . . # . . . . . . . # . . . . .]
; [# # # # # . . . # . . . . # . . # . . . . . . . . # # # . . . . . # # # . # . . # # # # # # . . # . . . . . . . # # # # # #]
; Took 10831 seconds to appear