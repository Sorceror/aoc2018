(ns aoc2018.day16
  (require [aoc2018.utils :as u]
           [clojure.string :as s]))

; --- Day 16: Chronal Classification ---
; The device has four registers (numbered 0 through 3) that can be manipulated by instructions containing one of 16 opcodes. The registers start with the value 0.
; Every instruction consists of four values: an opcode, two inputs (named A and B), and an output (named C), in that order. The opcode specifies the behavior of the instruction and how the inputs are interpreted. The output, C, is always treated as a register.
; Unfortunately, while the manual gives the name of each opcode, it doesn't seem to indicate the number. However, you can monitor the CPU to see the contents of the registers before and after instructions are executed to try to work them out. Each opcode has a number from 0 through 15, but the manual doesn't say which is which.
; How many samples in your puzzle input behave like three or more opcodes?

(def part1-regex #"(?m)^Before:\s+(\[\d+\,\s+\d+\,\s+\d+\,\s+\d+\])$\n^(\d+)\s+(\d+)\s+(\d+)\s+(\d+)$\n^After:\s+(\[\d+\,\s+\d+\,\s+\d+\,\s+\d+\])$")
(def part2-regex #"(\d+)\s+(\d+)\s+(\d+)\s+(\d+)")

(def input (u/resource->single-string "day16.txt"))

(def ops
  {:addr (fn [[a b c] r] (assoc r c (+ (get r a) (get r b))))
   :addi (fn [[a b c] r] (assoc r c (+ (get r a) b)))
   :mulr (fn [[a b c] r] (assoc r c (* (get r a) (get r b))))
   :muli (fn [[a b c] r] (assoc r c (* (get r a) b)))
   :banr (fn [[a b c] r] (assoc r c (bit-and (get r a) (get r b))))
   :bani (fn [[a b c] r] (assoc r c (bit-and (get r a) b)))
   :boor (fn [[a b c] r] (assoc r c (bit-or (get r a) (get r b))))
   :bori (fn [[a b c] r] (assoc r c (bit-or (get r a) b)))
   :setr (fn [[a _ c] r] (assoc r c (get r a)))
   :seti (fn [[a _ c] r] (assoc r c a))
   :gtir (fn [[a b c] r] (assoc r c (if (> a (get r b)) 1 0)))
   :gtri (fn [[a b c] r] (assoc r c (if (> (get r a) b) 1 0)))
   :gtrr (fn [[a b c] r] (assoc r c (if (> (get r a) (get r b)) 1 0)))
   :eqir (fn [[a b c] r] (assoc r c (if (= a (get r b)) 1 0)))
   :eqri (fn [[a b c] r] (assoc r c (if (= (get r a) b) 1 0)))
   :eqrr (fn [[a b c] r] (assoc r c (if (= (get r a) (get r b)) 1 0)))})

(defn parse-input [input]
  (->> input
      (re-seq part1-regex)
      (map (fn [[_ reg-before op-code input-a input-b input-c reg-after]]
             {:before (read-string reg-before)
              :after (read-string reg-after)
              :inputs [(u/s->i input-a) (u/s->i input-b) (u/s->i input-c)]
              :op-code (u/s->i op-code)}))))

(defn is-op [cpu-state [op-id op]]
  (if (= (get cpu-state :after) (op (get cpu-state :inputs) (get cpu-state :before)))
      op-id
      nil))

(defn behaves-as-ops [cpu-state operations]
  (->>
      (map #(is-op cpu-state %) operations)
      (filter some?)))

(defn count-op-codes-with-more-possibilities [input min-count]
  (->>
      (parse-input input)
      (map #(behaves-as-ops % ops))
      (map count)
      (filter #(>= % min-count))
      (count)))

(comment
  (println "Number of samples that behave like three or more op codes is" (count-op-codes-with-more-possibilities input 3)))
; Number of samples that behave like three or more op codes is 529

; --- Part Two ---
; Using the samples you collected, work out the number of each opcode and execute the test program (the second section of your puzzle input).
; What value is contained in register 0 after executing the test program?

(defn resolve-op-codes [input]
  (let [behaviors (distinct (map (fn [cpu-state] [(get cpu-state :op-code) (behaves-as-ops cpu-state ops)]) input))]
    (loop [known {}]
      (if (not= (count known) 16)
        (let [known-ids (keys known)
              known-ops (distinct (vals known))]
          (->> behaviors
               ; remove known op codes from behaviors list
               (filter #(every? (fn [k] (not= (first %) k)) known-ids))
               ; remove known ops from unknown op codes
               (map (fn [[op-code ops]] [op-code (filter (fn [op] (every? (fn [known-op] (not= known-op op)) known-ops)) ops)]))
               (distinct)
               (filter #(= (count (second %)) 1))
               (reduce (fn [known [op-code op-ids]] (assoc known op-code (first op-ids))) known)
               (recur)))
        known))))

(defn input->ops-map [input]
  (->> input
       (parse-input)
       (resolve-op-codes)
       (map (fn [[op-code op-id]] [op-code (get ops op-id)]))
       (into {})))

(defn parse-program [input]
  (->> input
       (#(s/split % #"\n\n\n\n"))
       (second)
       (re-seq part2-regex)
       (map (fn [[_ op-code a b c]] [(u/s->i op-code) [(u/s->i a) (u/s->i b) (u/s->i c)]]))))

(defn run-program [input registers]
  (let [program (parse-program input)
        ops-map (input->ops-map input)]
    (reduce (fn [r [op-code inputs]] ((get ops-map op-code) inputs r)) registers program)))

(comment
  (println "Content of register 0 after executing the test program" (first (run-program input [0 0 0 0]))))
; Content of register 0 after executing the test program 573
