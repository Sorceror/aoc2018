(ns aoc2018.day02
  (require [aoc2018.utils :as utils]))

; --- Day 2: Inventory Management System ---
; To make sure you didn't miss any, you scan the likely candidate boxes again, counting the number that have an ID containing exactly two of any letter and then separately counting those with exactly three of any letter. You can multiply those two counts together to get a rudimentary checksum and compare it to what your device predicts.
;
; For example, if you see the following box IDs:
;
; abcdef contains no letters that appear exactly two or three times.
; bababc contains two a and three b, so it counts for both.
; abbcde contains two b, but no letter appears exactly three times.
; abcccd contains three c, but no letter appears exactly two times.
; aabcdd contains two a and two d, but it only counts once.
; abcdee contains two e.
; ababab contains three a and three b, but it only counts once.
; Of these box IDs, four of them contain a letter which appears exactly twice, and three of them contain a letter which appears exactly three times. Multiplying these together produces a checksum of 4 * 3 = 12.
;
; What is the checksum for your list of box IDs?

(def input
  (utils/resource->strings "day02.txt"))

(defn checksum [input]
  (->> input
       (map frequencies)
       ; filter only 2 and 3 from frequencies
       (map #(filter (fn [[_ v]] (or (= v 2) (= v 3))) %))
       ; for each ID return [1 0] if only 2, [0 1] if only 3, [1 1] if 2 and 3
       (map #(reduce (fn [[twos threes] v] (if (= (second v) 2) [1 threes] [twos 1])) [0 0] %1))
       ; count 2s and 3s separately => [248 26]
       (reduce (fn [[twos threes] v] [(+ twos (first v)) (+ threes (second v))]))
       (apply *)))

(checksum input)
; 6448

; --- Part Two ---
; Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.
;
; The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:
;
; abcde
; fghij
; klmno
; pqrst
; fguij
; axcye
; wvxyz
; The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.
;
; What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing character from either ID, producing fgij.)

(defn group-chars [s1 s2]
  (->>
    (interleave s1 s2)
    (partition 2)))

(defn distance [s1 s2]
  (reduce
   (fn [acc [v1 v2]] (if (= v1 v2) acc (inc acc))) 0
   (group-chars s1 s2)))

(defn extract-same [s1 s2]
  (reduce
    (fn [acc [v1 v2]] (if (= v1 v2) (str acc v1) acc)) ""
    (group-chars s1 s2)))

(defn get-one-distance [s coll]
  (reduce
    (fn [has-one-dist current]
      (if (= (distance s current) 1) current has-one-dist))
    nil coll))

(defn find-common-letters [input]
  (loop [id (first input)
         to-see (rest input)]
    (if-let [has-one (get-one-distance id to-see)]
      (extract-same has-one id)
      (recur (first to-see) (rest to-see)))))

(find-common-letters (map seq input))
; "evsialkqyiurohzpwucngttmf"