(ns aoc2018.day07
  (require [aoc2018.utils :as u]
           [clojure.set :as set]))

; --- Day 7: The Sum of Its Parts ---
; The instructions specify a series of steps and requirements about which steps must be finished before others can begin (your puzzle input). Each step is designated by a single letter.
; In what order should the steps in your instructions be completed?

(def input
  (u/resource->strings "day07.txt"))

(def test-input
  (u/string->strings
    "Step C must be finished before step A can begin.
     Step C must be finished before step F can begin.
     Step A must be finished before step B can begin.
     Step A must be finished before step D can begin.
     Step B must be finished before step E can begin.
     Step D must be finished before step E can begin.
     Step F must be finished before step E can begin."))

(defn calculate-edges [input]
  (->> input
       (map #(re-find #"Step.(.).*step.(.).*" %))
       (map #(drop 1 %))
       (into #{})))

(defn calculate-roots [edges]
  (let [start-vs (into #{} (map first edges))
        end-vs (into #{} (map second edges))]
    (set/difference start-vs end-vs)))

(defn find-available-for-vertex [v edges]
  (->> edges
       (filter (fn [[sv _]] (= sv v)))
       (map second)
       (into (sorted-set))))

(defn find-available-for-selected-vertices [vertices edges]
  (->> vertices
       (map #(find-available-for-vertex % edges))
       (reduce set/union)
       ; input three has multiple roots, even though example does not show it or mention it :|
       (set/union (calculate-roots edges))
       (#(set/difference % vertices))))

(defn find-parents [v edges]
  (->> edges
       (filter (fn [[_ end-v]] (= end-v v)))
       (map first)
       (into #{})))

(defn find-reachable-for-vertices [seen edges]
  (->> edges
       (find-available-for-selected-vertices seen)
       (filter #(set/subset? (find-parents % edges) seen))
       (into (sorted-set))))

(defn find-steps [input]
  (let [edges (calculate-edges input)
        root (first (calculate-roots edges))]
    (loop [seen #{root}
           path (vector root)]
      (let [reachable (find-reachable-for-vertices seen edges)]
        (if (empty? reachable)
            path
            (recur (conj seen (first reachable)) (conj path (first reachable))))))))

(comment
  (->> input
       (find-steps)
       (reduce str)
       (println "Order of steps")))
; Order of steps LAPFCRGHVZOTKWENBXIMSUDJQY

(find-reachable-for-vertices #{} (calculate-edges input))

; --- Part Two ---
; Now, you need to account for multiple people working on steps simultaneously. If multiple steps are available, workers should still begin them in alphabetical order.
;
; Each step takes 60 seconds plus an amount corresponding to its letter: A=1, B=2, C=3, and so on. So, step A takes 60+1=61 seconds, while step Z takes 60+26=86 seconds. No time is required between steps.
; With 5 workers and the 60+ second step durations described above, how long will it take to complete all of the steps?

(defn create-workers [amount]
  (repeatedly amount #(vector 0 nil)))

(defn work [[time job]]
  [(if (> time 0) (dec time) 0) job])

(defn extract-done-jobs [workers]
  (->> workers
       (filter (fn [[time job]] (and (= time 0) (some? job))))
       (map second)
       (into #{})))

(defn remove-done-jobs [workers]
  (->> workers
       (map (fn [[time job]] (if (= time 0) [0 nil] [time job])))
       (into [])))

(defn jobs-in-progress [workers]
  (->> workers
       (filter #(some? (second %)))
       (map second)
       (into (sorted-set))))

(defn assign-job [job]
  [(- (int (first (char-array job))) 4) job])

(defn assign-jobs [jobs workers]
  (->> workers
       (reduce (fn [[ws js] [time job]]
                 (if (and (= time 0) (not-empty js))
                     [(conj ws (assign-job (first js))) (rest js)]
                     [(conj ws [time job]) js]))
               [[] (sort jobs)])
       (first)))

(defn find-count-with-workers [worker-count input]
  (let [edges (calculate-edges input)]
    (loop [iter 0
           seen #{}
           workers (create-workers worker-count)]
      (let [after-work (map work workers)
            after-seen (set/union seen (extract-done-jobs after-work))
            without-done (remove-done-jobs after-work)
            in-progress (jobs-in-progress without-done)
            reachable (find-reachable-for-vertices after-seen edges)
            non-assigned-reachable (set/difference reachable in-progress)
            busy-workers (assign-jobs non-assigned-reachable without-done)]
        (if (and (empty? reachable) (empty? (jobs-in-progress busy-workers)))
            iter
            (recur (inc iter) after-seen busy-workers))))))

(comment
  (println "Number of seconds until steps are finished with 5 workers is" (find-count-with-workers 5 input)))
; Number of seconds until steps are finished with 5 workers is 936