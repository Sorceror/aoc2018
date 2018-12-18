(ns aoc2018.day18
  (require [aoc2018.utils :as u]))

; --- Day 18: Settlers of The North Pole ---
; The lumber collection area is 50 acres by 50 acres; each acre can be either open ground (.), trees (|), or a lumberyard (#). You take a scan of the area (your puzzle input).
; Strange magic is at work here: each minute, the landscape looks entirely different. In exactly one minute, an open acre can fill with trees, a wooded acre can be converted to a lumberyard, or a lumberyard can be cleared to open ground (the lumber having been sent to other projects).
; The change to each acre is based entirely on the contents of that acre as well as the number of open, wooded, or lumberyard acres adjacent to it at the start of each minute. Here, "adjacent" means any of the eight acres surrounding that acre. (Acres on the edges of the lumber collection area might have fewer than eight adjacent acres; the missing acres aren't counted.)
;
; In particular:
; - An open acre will become filled with trees if three or more adjacent acres contained trees. Otherwise, nothing happens.
; - An acre filled with trees will become a lumberyard if three or more adjacent acres were lumberyards. Otherwise, nothing happens.
; - An acre containing a lumberyard will remain a lumberyard if it was adjacent to at least one other lumberyard and at least one acre containing trees. Otherwise, it becomes open.
; - These changes happen across all acres simultaneously, each of them using the state of all acres at the beginning of the minute and changing to their new form by the end of that same minute. Changes that happen during the minute don't affect each other.
;
; Multiplying the number of wooded acres by the number of lumberyards gives the total resource value after ten minutes.
; What will the total resource value of the lumber collection area be after 10 minutes?

(def input
  (u/resource->strings "day18.txt"))

(def pos-open \.)
(def pos-tree \|)
(def pos-yard \#)

(defn parse-input [input]
  (let [max-y (count input)
        max-x (count (first input))
        pos (->>
                (map #(map-indexed vector %) input)
                (map-indexed (fn [y row] (map (fn [[x c]] [[x y] c]) row)))
                (apply concat)
                (into (hash-map)))]
    (hash-map :size-x max-x :size-y max-y :positions pos)))

(defn get-val-at [x y state]
  (-> state
      (get :positions)
      (get [x y])))

(defn print-state [state]
  (->>
      (map
        (fn [y]
          (reduce
            (fn [s x] (str s (get-val-at x y state)))
            ""
            (for [x (range (get state :size-x))] x)))
        (for [y (range (get state :size-y))] y))
      (interpose \newline)
      (apply str)
      (println)))

(defn neighborhood [x y state]
  (->>
      [[(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)] [(dec x) y] [(inc x) y] [(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]]
      (map (fn [[x y]] (get-val-at x y state)))
      (filter some?)))

(defn next-for [type hood-freq]
  (cond
    (= type pos-open) (if (>= (get hood-freq pos-tree 0) 3)
                          pos-tree
                          pos-open)
    (= type pos-tree) (if (>= (get hood-freq pos-yard 0) 3)
                          pos-yard
                          pos-tree)
    (= type pos-yard) (if (and (>= (get hood-freq pos-tree 0) 1) (>= (get hood-freq pos-yard 0) 1))
                          pos-yard
                          pos-open)))

(defn step-state [state]
  (->>
      (map
        (fn [[[x y] c]]
          [[x y ] (->>
                    (neighborhood x y state)
                    (frequencies)
                    (next-for c))])
        (get state :positions))
      (into (hash-map))
      (assoc-in state [:positions])))

(defn step-state-times [num init-state]
  (loop [state init-state
         step 0]
    (if (>= step num)
        state
        (recur (step-state state) (inc step)))))

(defn count-wood [state]
  (->> (get state :positions)
       (filter (fn [[_ type]] (or (= type pos-tree) (= type pos-yard))))
       (map (fn [[_ type]] type))
       (frequencies)))

(comment
  (let [wood-freq (->> input
                       (parse-input)
                       (step-state-times 10)
                       (count-wood))]
    (println "Total resource value of lumber collection area after 10 minutes is"
             (* (get wood-freq pos-tree 0) (get wood-freq pos-yard 0)) "with" (get wood-freq pos-tree 0) "trees and" (get wood-freq pos-yard 0) "lumberyards")))
; Total resource value of lumber collection area after 10 minutes is 763804 with 1028 trees and 743 lumberyards

; --- Part Two ---
; This important natural resource will need to last for at least thousands of years. Are the Elves collecting this lumber sustainably?
; What will the total resource value of the lumber collection area be after 1000000000 minutes?

(comment
  (loop [state (parse-input input)
         step 0]
    (if (>= step 1000)
        nil
        (do
          (println step (sort (count-wood state)))
          (recur (step-state state) (inc step))))))
; After 450+ states it became stable and has repeating frequency 28
; (1000000000 - 524) mod 28 => 0 (meaning 524th step will yield same result as 1000000000th)

(comment
  (time (let [wood-freq (->> input
                             (parse-input)
                             (step-state-times 524)
                             (count-wood))]
          (println "Total resource value of lumber collection area after 1000000000th minutes is"
                   (* (get wood-freq pos-tree 0) (get wood-freq pos-yard 0)) "with" (get wood-freq pos-tree 0) "trees and" (get wood-freq pos-yard 0) "lumberyards"))))
; Total resource value of lumber collection area after 1000000000th minutes is 188400 with 600 trees and 314 lumberyards
; "Elapsed time: 8699.0804 msecs" ~ 8s