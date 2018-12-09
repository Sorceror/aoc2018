(ns aoc2018.day09
  (require [aoc2018.utils :as u]
           [clojure.data.finger-tree :as ft])
  (:import (java.util LinkedList)))

;--- Day 9: Marble Mania ---
; The Elves play this game by taking turns arranging the marbles in a circle according to very particular rules. The marbles are numbered starting with 0 and increasing by 1 until every marble has a number.
; First, the marble numbered 0 is placed in the circle. At this point, while it contains only a single marble, it is still a circle: the marble is both clockwise from itself and counter-clockwise from itself. This marble is designated the current marble.
; Then, each Elf takes a turn placing the lowest-numbered remaining marble into the circle between the marbles that are 1 and 2 marbles clockwise of the current marble. (When the circle is large enough, this means that there is one marble between the marble that was just placed and the current marble.) The marble that was just placed then becomes the current marble.
; However, if the marble that is about to be placed has a number which is a multiple of 23, something entirely different happens. First, the current player keeps the marble they would have placed, adding it to their score. In addition, the marble 7 marbles counter-clockwise from the current marble is removed from the circle and also added to the current player's score. The marble located immediately clockwise of the marble that was removed becomes the new current marble.
; What is the winning Elf's score? <- part 1
; What would the new winning Elf's score be if the number of the last marble were 100 times larger? <- part 2

(def input
  (->>
      (u/resource->strings "day09.txt")
      (first)
      (re-find #"(\d+).*worth.(\d+).*")
      (rest)
      (map u/s->i)))

; -------- PURE CLOJURE VEC APPROACH --------

(defn clockwise [current total circle]
  (mod (+ current total) (count circle)))

(defn counter-clockwise [current total circle]
  (mod (- current total) (count circle)))

(defn place-after [pos value circle]
  (apply conj (vec (take (inc pos) circle)) value (vec (drop (inc pos) circle))))

(defn extract-on [pos circle]
  [(first (drop pos circle))
   (apply conj (vec (take pos circle)) (vec (drop (inc pos) circle)))])

(defn run-game [num-players last-marble-worth]
  (loop [current-idx 0
         current-marble 1
         circle [0]
         player-id 0
         players (vec (repeat num-players 0))]
    (if (zero? (mod current-marble 5000)) (println current-marble))
    (if (= (mod current-marble 23) 0)
      (let [new-idx (counter-clockwise current-idx 7 circle)
            [value new-circle] (extract-on new-idx circle)]
        (recur new-idx (inc current-marble) new-circle (clockwise player-id 1 players) (assoc players player-id (+ (get players player-id) value current-marble))))
      (let [pos (clockwise current-idx 1 circle)
            new-circle (place-after pos current-marble circle)]
        (if (> current-marble last-marble-worth)
          players
          (recur (inc pos) (inc current-marble) new-circle (clockwise player-id 1 players) players))))))

(comment
  (time (apply max (run-game 30 5807)))
  ;"Elapsed time: 1400.539339 msecs"
  ;=> 37305
  (time (apply max (run-game (first input) (second input)))))
  ; "Elapsed time: 211587.404153 msecs" ~ 3.5 min
  ; => 394486)

; -------- CONSTANT ARRAY SIZE APPROACH --------

(defn clockwise-array [current total size]
  (mod (+ current total) size))

(defn counter-clockwise-array [current total size]
  (mod (- current total) size))

(defn place-after-array [pos value circle size]
  (->
      (reduce (fn [new-circle idx] (assoc new-circle idx (get new-circle (dec idx)))) circle (range size pos -1))
      (assoc (inc pos) value)))

(defn extract-on-array [pos circle size]
  [(nth circle pos)
   (assoc (reduce (fn [new-circle idx] (assoc new-circle idx (get new-circle (inc idx)))) circle (range pos size)) (dec size) -1)])

(defn run-game-array [num-players last-marble-worth]
  (loop [current-idx 0
         current-marble 1
         circle (assoc (vec (repeat last-marble-worth -1)) 0 0)
         size 1
         player-id 0
         players (vec (repeat num-players 0))]
    (if (= (mod current-marble 23) 0)
      (let [new-idx (counter-clockwise-array current-idx 7 size)
            [value new-circle] (extract-on-array new-idx circle size)]
        (recur
          new-idx (inc current-marble)
          new-circle (dec size)
          (clockwise-array player-id 1 num-players) (assoc players player-id (+ (get players player-id) value current-marble))))
      (let [pos (clockwise-array current-idx 1 size)
            new-circle (place-after-array pos current-marble circle size)]
        (if (> current-marble last-marble-worth)
          players
          (recur
            (inc pos) (inc current-marble)
            new-circle (inc size)
            (clockwise-array player-id 1 num-players) players))))))

(comment
  (time (apply max (run-game-array 30 5807)))
  ; "Elapsed time: 1189.956176 msecs"
  ; => 37305

  ; takes forever to compute
  (time (apply max (run-game-array (first input) (second input)))))
  ; "Elapsed time: 171458.652593 msecs" ~ 3 min
  ; => 394486

; -------- JAVA LINKED LIST APPROACH --------

(defn clockwise-list [^long current ^long total ^LinkedList circle-list]
  (mod (+ current total) (.size circle-list)))

(defn counter-clockwise-list [^long current ^long total ^LinkedList circle-list]
  (mod (- current total) (.size circle-list)))

(defn place-after-list [^long pos ^long value ^LinkedList circle-list]
  (.add circle-list (inc pos) value))

(defn extract-on-list [^long pos ^LinkedList circle-list]
  (.remove circle-list pos))

(defn run-game-list
  ([num-players last-marble-worth] (let [list (LinkedList.) _ (.add list 0)] (run-game-list num-players last-marble-worth list)))
  ([num-players last-marble-worth circle-list]
   (loop [current-idx 0
          current-marble 1
          circle ^LinkedList circle-list
          player-id 0
          players (vec (repeat num-players 0))]
     (if (= (mod current-marble 23) 0)
       (let [new-idx (counter-clockwise-list current-idx 7 circle)
             value (extract-on-list new-idx circle)]
         (recur new-idx (inc current-marble) circle (mod (inc player-id) num-players) (assoc players player-id (+ (get players player-id) value current-marble))))
       (let [pos (clockwise-list current-idx 1 circle)
             _ (place-after-list pos current-marble circle)]
         (if (> current-marble last-marble-worth)
             players
             (recur (inc pos) (inc current-marble) circle (mod (inc player-id) num-players) players)))))))

(comment
  (time (apply max (run-game-list 30 5807)))
  ; "Elapsed time: 17.24249 msecs"
  ; => 37305
  (time (apply max (run-game-list (first input) (second input))))
  ; "Elapsed time: 6042.217532 msecs" ~ 6 sec
  ; => 394486
  (time (apply max (run-game-list (first input) (* (second input) 100)))))
  ; Wasn't done after 40 minutes :(

; -------- FINGER TREES - COUNTED-DOUBLE-LIST APPROACH --------

(defn clockwise-tree [current total circle]
  (mod (+ current total) (count circle)))

(defn counter-clockwise-tree [current total circle]
  (mod (- current total) (count circle)))

(defn place-after-tree [pos value circle]
  (let [[l v r] (ft/ft-split-at circle pos)]
    (ft/ft-concat (conj l v value) r)))

(defn run-game-tree [num-players last-marble-worth]
  (loop [current-idx 0
         current-marble 1
         circle (ft/counted-double-list 0)
         player-id 0
         players (vec (repeat num-players 0))]
    (if (< current-marble last-marble-worth)
        (if (= (mod current-marble 23) 0)
            (let [new-idx (counter-clockwise-tree current-idx 7 circle)
                  [left value right] (ft/ft-split-at circle new-idx)]
              (recur
                new-idx
                (inc current-marble)
                (ft/ft-concat left right)
                (clockwise-tree player-id 1 players)
                (assoc players player-id (+ (get players player-id) value current-marble))))
            (let [pos (clockwise-tree current-idx 1 circle)]
              (recur
                (inc pos)
                (inc current-marble)
                (place-after-tree pos current-marble circle)
                (clockwise-tree player-id 1 players)
                players)))
        players)))

(comment
  (time (apply max (run-game-tree 30 5807)))
  ; "Elapsed time: 182.845056 msecs"
  ; => 37305
  (time (apply max (run-game-tree (first input) (second input))))
  ; "Elapsed time: 2385.809445 msecs" ~ 3 sec
  ; => 394486
  (time (apply max (run-game-tree (first input) (* (second input) 100)))))
  ; "Elapsed time: 393548.41902 msecs" ~ 6.5 min
  ; => 3276488008