(ns aoc2018.day12
  (require [aoc2018.utils :as u]
           [clojure.string :as str]))

; --- Day 12: Subterranean Sustainability ---
; After exploring a little, you discover a long tunnel that contains a row of small pots as far as you can see to your left and right.
; The pots are numbered, with 0 in front of you. To the left, the pots are numbered -1, -2, -3, and so on; to the right, 1, 2, 3.... Your puzzle input contains a list of pots from 0 to the right and whether they do (#) or do not (.) currently contain a plant, the initial state. (No other pots currently contain plants.) For example, an initial state of #..##.... indicates that pots 0, 3, and 4 currently contain plants.
; Your puzzle input also contains some notes you find on a nearby table: someone has been trying to figure out how these plants spread to nearby pots. Based on the notes, for each generation of plants, a given pot has or does not have a plant based on whether that pot (and the two pots on either side of it) had a plant in the last generation. These are written as LLCRR => N, where L are pots to the left, C is the current pot being considered, R are the pots to the right, and N is whether the current pot will have a plant in the next generation.
; After 20 generations, what is the sum of the numbers of all pots which contain a plant?

(def input
  (u/resource->strings "day12.txt"))

(defn seq->booleans [s]
  (map #(= \# %) s))

(defn parse-initial-state [input]
  (->> input
       (first)
       (re-find #"initial state: (.*)")
       (second)
       (seq->booleans)
       (map-indexed hash-map)
       (into (hash-map))))

(defn parse-rules [input]
  (->> input
       (drop 2)
       (map #(str/split % #" => "))
       (map (fn [[rule-str char-str]] [(seq->booleans rule-str) (first (seq->booleans char-str))]))))

(defn get-pos [idx state]
  (if-let [v (get state idx)]
    v
    false))

(defn find-bounds [state]
  (let [trues (map first (filter (fn [[_ v]] (true? v)) state))]
    [(apply min trues) (apply max trues)]))

(defn apply-rules [idx state rules]
  (let [hood (list
               (get-pos (- idx 2) state)
               (get-pos (- idx 1) state)
               (get-pos idx state)
               (get-pos (+ idx 1) state)
               (get-pos (+ idx 2) state))
        rule (first (filter #(= (first %) hood) rules))]
    (if (nil? rule)
        false
        (second rule))))

(defn iterate-generation [state rules]
  (let [[min-idx max-idx] (find-bounds state)
        min-idx (- min-idx 5)
        max-idx (+ max-idx 6)]
    (reduce (fn [new-state idx] (assoc new-state idx (apply-rules idx state rules)))
            (hash-map)
            (for [idx (range min-idx max-idx)] idx))))

(defn run-for-generations [num-gen initial-state rules]
  (loop [num 0
         state initial-state]
    (if (>= num num-gen)
        state
        (recur (inc num) (iterate-generation state rules)))))

(defn count-pots-with-flowers [state]
  (->> state
       (filter (fn [[_ v]] (true? v)))
       (map first)
       (reduce +)))

(comment
  (let [initial-state (parse-initial-state input)
        rules (parse-rules input)]
    (println "Sum of pots after 20 generations is" (count-pots-with-flowers (run-for-generations 20 initial-state rules)))))
; Sum of pots after 20 generations is 3241

; --- Part Two ---
; You realize that 20 generations aren't enough. After all, these plants will need to last another 1500 years to even reach your timeline, not to mention your future.
;
; After fifty billion (50000000000) generations, what is the sum of the numbers of all pots which contain a plant?

(comment
  (let [initial-state (parse-initial-state input)
        rules (parse-rules input)
        test-gen 1000]
    (println (- (count-pots-with-flowers (run-for-generations (+ 1 test-gen) initial-state rules))
                (count-pots-with-flowers (run-for-generations test-gen initial-state rules)))
             (- (count-pots-with-flowers (run-for-generations (+ 2 test-gen) initial-state rules))
                (count-pots-with-flowers (run-for-generations (+ 1 test-gen) initial-state rules)))
             (- (count-pots-with-flowers (run-for-generations (+ 3 test-gen) initial-state rules))
                (count-pots-with-flowers (run-for-generations (+ 2 test-gen) initial-state rules)))
             (- (count-pots-with-flowers (run-for-generations (+ 4 test-gen) initial-state rules))
                (count-pots-with-flowers (run-for-generations (+ 3 test-gen) initial-state rules)))
             (- (count-pots-with-flowers (run-for-generations (+ 5 test-gen) initial-state rules))
                (count-pots-with-flowers (run-for-generations (+ 4 test-gen) initial-state rules))))
    (println (count-pots-with-flowers (run-for-generations test-gen initial-state rules)))))
; 55 55 55 55 55
; 54911
; So it's stabilized after 1000 iterations on count 54911 with gen-diff 55

(comment
  (println "Sum of pots after 50000000000 iterations is" (+ (* (- 50000000000 1000) 55) 54911)))
; Sum of pots after 50000000000 iterations is 2749999999911

