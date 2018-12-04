(ns aoc2018.day04
  (require [aoc2018.utils :as u]))

; --- Day 4: Repose Record ---
;
; Following records, which have already been organized into chronological order:
;
; [1518-11-01 00:00] Guard #10 begins shift
; [1518-11-01 00:05] falls asleep
; [1518-11-01 00:25] wakes up
; [1518-11-01 00:30] falls asleep
; [1518-11-01 00:55] wakes up
; [1518-11-01 23:58] Guard #99 begins shift
; [1518-11-02 00:40] falls asleep
; [1518-11-02 00:50] wakes up
; [1518-11-03 00:05] Guard #10 begins shift
; [1518-11-03 00:24] falls asleep
; [1518-11-03 00:29] wakes up
; [1518-11-04 00:02] Guard #99 begins shift
; [1518-11-04 00:36] falls asleep
; [1518-11-04 00:46] wakes up
; [1518-11-05 00:03] Guard #99 begins shift
; [1518-11-05 00:45] falls asleep
; [1518-11-05 00:55] wakes up
; Timestamps are written using year-month-day hour:minute format. The guard falling asleep or waking up is always the one whose shift most recently started. Because all asleep/awake times are during the midnight hour (00:00 - 00:59), only the minute portion (00 - 59) is relevant for those events.
;
; Note that guards count as asleep on the minute they fall asleep, and they count as awake on the minute they wake up. For example, because Guard #10 wakes up at 00:25 on 1518-11-01, minute 25 is marked as awake.
;
; If you can figure out the guard most likely to be asleep at a specific time.
;
; Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the most?
; What is the ID of the guard you chose multiplied by the minute you chose?

(def input
  (u/resource->strings "day04.txt"))

(defn input->seq [input]
  (->> input
       (sort)
       (map #(re-find #"\[\d{4}-(\d{2})-(\d{2}).(\d{2}):(\d{2})\].(.*\#(\d+).*|.*(asleep).*|.*(up).*)" %1))))

(defn parse-log-line [log-line guard]
  (let [date (str (nth log-line 1) (nth log-line 2))
        time (str (nth log-line 3) (nth log-line 4))]
    (cond
      (some? (nth log-line 6)) [(nth log-line 6) nil]
      (some? (nth log-line 7)) [guard [date time :asleep]]
      (some? (nth log-line 8)) [guard [date time :up]])))

(defn fill-guard-log [input]
  (->> input
       (input->seq)
       (reduce (fn [[log guard] line]
                 (let [[new-guard line-data] (parse-log-line line guard)]
                   (if (nil? line-data)
                     [log new-guard]
                     [(conj log [new-guard line-data]) new-guard])))
               [[] nil])
       (first)
       (partition 2)
       (map (fn [[[g-id [date asleep _]] [_ [_ up]]]] (vector g-id date asleep up)))
       (map (fn [[g-id date asleep up]] (vector g-id date (into [] (range (Integer/parseInt asleep) (Integer/parseInt up))))))
       (group-by #(first %))))


(defn find-sleepiest-guard [log]
  (->> log
       (map (fn [[g-id g-log]]
              [g-id (reduce (fn [total [_ _ minutes]] (+ total (count minutes))) 0 g-log)]))
       (reduce (fn [[max-id max-count][g-id count]]
                 (if (> count max-count)
                     [g-id count]
                     [max-id max-count])) [nil 0])))

(defn find-sleepiest-minute [guard log]
  (->> (get log guard)
       (map (fn [[_ _ minutes]] minutes))
       (apply concat)
       (frequencies)
       (apply max-key val)))

(comment
  (let [guards-log (fill-guard-log input)]
    (let [[id count] (find-sleepiest-guard guards-log)
          [minute _] (find-sleepiest-minute id guards-log)]
      (println "Sleepiest guard is" id "with" count "minutes, his sleepiest minute is" minute "puzzle result is" (* (read-string id) minute)))))
; Sleepiest guard is 1571 with 493 minutes, his sleepiest minute is 54 , puzzle result is 84834

; --- Part Two ---
; Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?
; What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 99 * 45 = 4455.)
(defn find-most-frequent-asleep-minute [guard-log]
  (->>
    (map (fn [guard] [guard (find-sleepiest-minute guard guard-log)]) (keys guard-log))
    (into {})
    (apply max-key (comp val second))))

(comment
  (let [guard-log (fill-guard-log input)
        [id [minute total]] (find-most-frequent-asleep-minute guard-log)]
    (println "Most frequent asleep guard on the same minute is" id "on minute" minute "puzzle result is" (* (read-string id) minute))))
; Most frequent asleep guard on the same minute is 1619 on minute 33 puzzle result is 53427