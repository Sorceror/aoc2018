(ns aoc2018.day05
  (require [aoc2018.utils :as u]))

; --- Day 5: Alchemical Reduction ---
; The polymer is formed by smaller units which, when triggered, react with each other such that two adjacent units of the same type and opposite polarity are destroyed. Units' types are represented by letters; units' polarity is represented by capitalization. For instance, r and R are units with the same type but opposite polarity, whereas r and s are entirely different types and do not react.
;
; For example:
;
; In aA, a and A react, leaving nothing behind.
; In abBA, bB destroys itself, leaving aA. As above, this then destroys itself, leaving nothing.
; In abAB, no two adjacent units are of the same type, and so nothing happens.
; In aabAAB, even though aa and AA are of the same type, their polarities match, and so nothing happens.
; How many units remain after fully reacting the polymer you scanned?

(def input
  (into [] (first (u/resource->strings "day05.txt"))))


(def test-input (into [] "dabAcCaCBAcCcaDA"))

(defn do-react? [u1 u2]
  (if (or (nil? u1) (nil? u2))
      false
      (= (Math/abs (- (int u1) (int u2))) 32)))

(defn run-reaction [polymer-seq]
  (loop [polymer polymer-seq
         reacted-polymer-seq []]
    (if (empty? polymer)
      reacted-polymer-seq
      (let [u1 (first polymer)
            u2 (second polymer)]
        (if (do-react? u1 u2)
          (recur (drop 2 polymer) reacted-polymer-seq)
          (recur (drop 1 polymer) (conj reacted-polymer-seq u1)))))))

(defn run-reaction-loop [polymer-seq]
  (loop [current-polymer polymer-seq]
    (let [reacted-polymer (run-reaction current-polymer)]
      (if (= current-polymer reacted-polymer)
          current-polymer
          (recur reacted-polymer)))))

(comment
  (->>
      (run-reaction-loop input)
      (count)
      (println "Remaining count after all reactions is")))
; Remaining count after all reactions is 11720

; --- Part Two ---
; One of the unit types is causing problems; it's preventing the polymer from collapsing as much as it should. Your goal is to figure out which unit type is causing the most problems, remove all instances of it (regardless of polarity), fully react the remaining polymer, and measure its length.
; What is the length of the shortest polymer you can produce by removing all units of exactly one type and fully reacting the result?

(defn remove-from-polymer [polymer unit]
  (into [] (clojure.string/replace (reduce str polymer) (re-pattern (str "(?i)" unit)) "")))

(defn find-shortest-reduced-size [polymer-seq]
  (->>
      (range (int \A) (int \Z))
      (pmap #(->> %
                  (char)
                  (remove-from-polymer polymer-seq)
                  (run-reaction-loop)
                  (count)))
      (reduce min)))

(comment
  (->>
      (find-shortest-reduced-size input)
      (println "Shortest polymer size after reactions with reduced units is")))
; Shortest polymer size after reactions with reduced units is 4956

