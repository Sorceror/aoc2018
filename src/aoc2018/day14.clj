(ns aoc2018.day14
  (:import (java.util ArrayList)))

; --- Day 14: Chocolate Charts ---
; The Elves are trying to come up with the ultimate hot chocolate recipe; they're even maintaining a scoreboard which tracks the quality score (0-9) of each recipe.
; Only two recipes are on the board: the first recipe got a score of 3, the second, 7. Each of the two Elves has a current recipe: the first Elf starts with the first recipe, and the second Elf starts with the second recipe.
; To create new recipes, the two Elves combine their current recipes. This creates new recipes from the digits of the sum of the current recipes' scores. With the current recipes' scores of 3 and 7, their sum is 10, and so two new recipes would be created: the first with score 1 and the second with score 0. If the current recipes' scores were 2 and 3, the sum, 5, would only create one recipe (with a score of 5) with its single digit.
; The new recipes are added to the end of the scoreboard in the order they are created. So, after the first round, the scoreboard is 3, 7, 1, 0.
; After all new recipes are added to the scoreboard, each Elf picks a new current recipe. To do this, the Elf steps forward through the scoreboard a number of recipes equal to 1 plus the score of their current recipe. So, after the first round, the first Elf moves forward 1 + 3 = 4 times, while the second Elf moves forward 1 + 7 = 8 times. If they run out of recipes, they loop back around to the beginning. After the first round, both Elves happen to loop around until they land on the same recipe that they had in the beginning; in general, they will move to different recipes.
; What are the scores of the ten recipes immediately after the number of recipes in your puzzle input?

(defn move [pos total recipes]
  (mod (+ pos total) (count recipes)))

(defn create-recipe [pos1 pos2 recipes]
  (let [r (+ (nth recipes pos1) (nth recipes pos2))]
    (if (>= r 10)
        [(quot r 10) (mod r 10)]
        [r])))

(defn score-ten-after [total]
  (->>
    (loop [p1 0
           p2 1
           recipes [3 7]]
      (let [r (create-recipe p1 p2 recipes)
            new-recipes (apply conj recipes r)]
        (if (>= (count recipes) (+ total 10))
            new-recipes
            (recur
              (move p1 (+ (nth recipes p1) 1) new-recipes)
              (move p2 (+ (nth recipes p2) 1) new-recipes)
              new-recipes))))
    (drop total)
    (take 10)))


(comment
  (println "Score ten after 84601 recipes is" (apply str (score-ten-after 84601))))
; Score ten after 84601 recipes is 2688510125

; --- Part Two ---
; As it turns out, you got the Elves' plan backwards. They actually want to know how many recipes appear on the scoreboard to the left of the first recipes whose scores are the digits from your puzzle input.
; How many recipes appear on the scoreboard to the left of the score sequence in your puzzle input?

(defn val-at-list [pos ^ArrayList list]
  (.get list pos))

(defn create-and-add-new-recipe [pos1 pos2 ^ArrayList recipes]
  (let [r (+ (val-at-list pos1 recipes) (val-at-list pos2 recipes))]
    (if (>= r 10)
      (doto recipes
        (.add (quot r 10))
        (.add (mod r 10)))
      (.add recipes r))))

(defn compare-last [recipes s offset]
  (if (>= (.size recipes) (inc (count s)))
      (every? true?
        (map
          (fn [idx]
            (= (val-at-list (- (.size recipes) idx 1 offset) recipes) (nth s (- (count s) idx 1))))
          (range 0 (count s))))
      false))

(defn step-with-last-as-list [s]
  (loop [p1 0
         p2 1
         recipes (doto (ArrayList.) (.add 3) (.add 7))
         step 0]
    (create-and-add-new-recipe p1 p2 recipes)
    ; the sequence can be s steps back or s-1 steps back because 2 recipes can be created in single step
    (let [r1 (compare-last recipes s 0)
          r2 (compare-last recipes s 1)]
      (if (or (true? r1) (true? r2))
        (if (true? r1)
            (- (.size recipes) (count s))
            (- (.size recipes) (count s) 1))
        (recur
          (move p1 (+ (val-at-list p1 recipes) 1) recipes)
          (move p2 (+ (val-at-list p2 recipes) 1) recipes)
          recipes
          (inc step))))))

(comment
  (println (step-with-last-as-list [0 8 4 6 0 1]) "recipes will appear on the left of 084601"))
; "20188250 recipes will appear on the left of 084601"
; "Elapsed time: 337283.100504 msecs ~ 5.6 min"
