(ns aoc2018.day11)

;--- Day 11: Chronal Charge ---
; Each fuel cell has a coordinate ranging from 1 to 300 in both the X (horizontal) and Y (vertical) direction. In X,Y notation, the top-left cell is 1,1, and the top-right cell is 300,1.
;
; The interface lets you select any 3x3 square of fuel cells. To increase your chances of getting to your destination, you decide to choose the 3x3 square with the largest total power.
;
; The power level in a given fuel cell can be found through the following process:
; - Find the fuel cell's rack ID, which is its X coordinate plus 10.
; - Begin with a power level of the rack ID times the Y coordinate.
; - Increase the power level by the value of the grid serial number (your puzzle input).
; - Set the power level to itself multiplied by the rack ID.
; - Keep only the hundreds digit of the power level (so 12345 becomes 3); numbers with no hundreds digit become 0.
; - Subtract 5 from the power level.
;
; Your goal is to find the 3x3 square which has the largest total power. The square must be entirely within the 300x300 grid. Identify this square using the X,Y coordinate of its top-left fuel cell.

(def grid-serial 5535)

(defn generate-grid [grid-serial-number]
  (reduce
    (fn [grid [x y]]
      (conj grid
        (let [rack-id (+ x 10)]
          (- (rem (quot (* (+ (* rack-id y) grid-serial-number) rack-id) 100) 10) 5))))
    []
    (for [y (range 1 301) x (range 1 301)] [x y])))

(defn index [x y]
  (if (or (<= x 0) (<= y 0))
    -1
    (+ (* (dec y) 300) (dec x))))

(defn get-pos [x y grid]
  (let [idx (index x y)]
    (if (neg? idx)
        0
        (nth grid idx))))

(defn sum3x3 [x y grid]
  (reduce
    (fn [sum [x y]] (+ sum (get-pos x y grid)))
    0
    [[x y] [(+ x 1) y] [(+ x 2) y]
     [x (+ y 1)] [(+ x 1) (+ y 1)] [(+ x 2) (+ y 1)]
     [x (+ y 2)] [(+ x 1) (+ y 2)] [(+ x 2) (+ y 2)]]))

(defn find-max-3x3 [grid-serial]
  (let [grid (generate-grid grid-serial)]
   (reduce
     (fn [[max-sum max-x max-y] [x y]]
       (let [sum (sum3x3 x y grid)]
         (if (> sum max-sum)
             [sum x y]
             [max-sum max-x max-y])))
     [Integer/MIN_VALUE -1 -1]
     (for [y (range 1 299) x (range 1 299)] [x y]))))

(comment
  (let [[_ x y] (find-max-3x3 grid-serial)]
    (println "Grid top-left coordinates with max 3x3 sum is" (format "%d,%d" x y))))
; Grid top-left coordinates with max 3x3 sum is 19,41

; --- Part Two ---
; You now must find the square of any size with the largest total power.
; What is the X,Y,size identifier of the square with the largest total power?

; based on https://www.wikiwand.com/en/Summed-area_table
(defn generate-sum-table [grid]
  (reduce
    (fn [table [x y]]
      (assoc table (index x y)
                   (-
                     (+
                       (get-pos x y grid)
                       (get-pos (dec x) y table)
                       (get-pos x (dec y) table))
                     (get-pos (dec x) (dec y) table))))
    (apply vector (repeat (* 300 300) 0))
    (for [y (range 1 301) x (range 1 301)] [x y])))

(defn sumXxX [x y size sum-table]
  (-
    (+
      (get-pos (+ x (dec size)) (+ y (dec size)) sum-table)
      (get-pos (dec x) (dec y) sum-table))
    (get-pos (+ x (dec size)) (dec y) sum-table)
    (get-pos (dec x) (+ y (dec size)) sum-table)))

(defn max-sub-grid [size sum-table]
  (reduce
    (fn [[max-sum max-x max-y] [x y]]
      (let [sum (sumXxX x y size sum-table)]
        (if (> sum max-sum)
          [sum x y]
          [max-sum max-x max-y])))
    [Integer/MIN_VALUE -1 -1]
    (for [y (range 1 (- 302 size)) x (range 1 (- 302 size))] [x y])))

(defn find-max-anyxany [grid-serial]
  (let [table (generate-sum-table (generate-grid grid-serial))]
    (reduce
      (fn [[max-sum max-size max-x max-y] size]
        (let [[sum x y] (max-sub-grid size table)]
          (if (> sum max-sum)
            [sum size x y]
            [max-sum max-size max-x max-y])))
      [0 0 -1 -1]
      (for [size (range 2 301)] size))))

(comment
  (let [[_ size x y] (find-max-anyxany grid-serial)]
    (println "Grid top-left coordinates with max area sum and it's size is" (format "%d,%d,%d" x y size)))
  (time (find-max-anyxany grid-serial)))
; Grid top-left coordinates with max area sum and it's size is 237,284,11
; "Elapsed time: 3026.658402 msecs"
; => [91 11 237 284]
