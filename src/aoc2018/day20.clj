(ns aoc2018.day20
  (require [aoc2018.utils :as u]))

; --- Day 20: A Regular Map ---
; When you look up, you discover that the North Pole base construction project has completely surrounded you.
; The area you are in is made up entirely of rooms and doors. The rooms are arranged in a grid, and rooms only connect to adjacent rooms when a door is present between them.
; You get the attention of a passing construction Elf and ask for a map. "I don't have time to draw out a map of this place - it's huge. Instead, I can give you directions to every room in the facility!" He writes down some directions on a piece of parchment and runs off. In the example above, the instructions might have been ^WNE$, a regular expression or "regex" (your puzzle input).
; The regex matches routes (like WNE for "west, north, east") that will take you from your current room through various doors in the facility. In aggregate, the routes will take you through every door in the facility at least once; mapping out all of these routes will let you build a proper map and find your way around.
; ^ and $ are at the beginning and end of your regex; these just mean that the regex doesn't match anything outside the routes it describes. (Specifically, ^ matches the start of the route, and $ matches the end of it.) These characters will not appear elsewhere in the regex.
; The rest of the regex matches various sequences of the characters N (north), S (south), E (east), and W (west). In the example above, ^WNE$ matches only one route, WNE, which means you can move west, then north, then east from your current position. Sequences of letters like this always match that exact route in the same order.
; Sometimes, the route can branch. A branch is given by a list of options separated by pipes (|) and wrapped in parentheses. So, ^N(E|W)N$ contains a branch: after going north, you must choose to go either east or west before finishing your route by going north again. By tracing out the possible routes after branching, you can determine where the doors are and, therefore, where the rooms are in the facility.
; What is the largest number of doors you would be required to pass through to reach a room?

(def input (u/resource->single-string "day20.txt" true))

(def init-stack '())
(defn push-to-stack [v stack]
  (conj stack v))
(defn peek-stack [stack]
  (first stack))
(defn pop-stack [stack]
  (rest stack))

(def init-pos [0 0])
(defn move-point [[x y] direction]
  (cond
    (= direction \E) [(inc x) y]
    (= direction \W) [(dec x) y]
    (= direction \N) [x (inc y)]
    (= direction \S) [x (dec y)]
    :else (println "Invalid move direction" direction)))

(def init-vertices {init-pos {:id 0 :dist 0} :last-id 0})
(defn get-vertex [pos dist vertices]
  (let [new-vertices (if (some? (get vertices pos))
                       vertices
                       (let [tmp (update-in vertices [:last-id ] inc)]
                         (assoc tmp pos {:id (get tmp :last-id) :dist dist})))]
    [(get new-vertices pos) new-vertices]))

(defn parse-input [input]
  (loop [pos-stack init-stack
         vertices init-vertices
         s input
         pos init-pos]
    (let [c (first s)
          new-s (rest s)]
      (cond
        (= c \^) (recur pos-stack vertices new-s pos)
        (= c \$) vertices
        (= c \() (recur (push-to-stack pos pos-stack) vertices new-s pos)
        (= c \|) (recur pos-stack vertices new-s (peek-stack pos-stack))
        (= c \)) (recur (pop-stack pos-stack) vertices new-s pos)
        :else (let [new-pos (move-point pos c)
                    [old-vertex _] (get-vertex pos -1 vertices)
                    [_ new-vertices] (get-vertex new-pos (inc (get old-vertex :dist)) vertices)]
                (recur pos-stack new-vertices new-s new-pos))))))

(comment
  (println "Largest number of doors to pass through to reach a room is"
    (->>
        (parse-input input)
        (#(dissoc % :last-id))
        (map (fn [[_ v]] (get v :dist)))
        (apply max))))
; Largest number of doors to pass through to reach a room is 3739

; --- Part Two ---
; How many rooms have a shortest path from your current location that pass through at least 1000 doors?

(comment
  (println "Room count with shortest path at the least 1000 doors is"
    (->>
      (parse-input input)
      (#(dissoc % :last-id))
      (map (fn [[_ v]] (get v :dist)))
      (filter #(>= % 1000))
      (count))))
; Room count with shortest path at the least 1000 doors is 8409