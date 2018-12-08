(ns aoc2018.day08
  (require [aoc2018.utils :as u]))

; --- Day 8: Memory Maneuver ---
; The navigation system's license file consists of a list of numbers (your puzzle input). The numbers define a data structure which, when processed, produces some kind of tree that can be used to calculate the license number.
;
; The tree is made up of nodes; a single, outermost node forms the tree's root, and it contains all other nodes in the tree (or contains nodes that contain nodes, and so on).
;
; Specifically, a node consists of:
;
; - A header, which is always exactly two numbers:
;   - The quantity of child nodes.
;   - The quantity of metadata entries.
; - Zero or more child nodes (as specified in the header).
; - One or more metadata entries (as specified in the header).
; Each child node is itself a node that has its own header, child nodes, and metadata.
; The first check done on the license file is to simply add up all of the metadata entries.
;
; What is the sum of all metadata entries?

(def input
  (u/resource->ints "day08.txt"))

(defn parse-seq [[seq tree]]
  (let [child-nodes-count (first seq)
        metadata-count (second seq)
        [post-child-seq all-children] (reduce (fn [[new-seq children] _]
                                                (let [[post-seq new-children] (parse-seq [new-seq (vector)])]
                                                  [post-seq (into [] (conj children new-children))]))
                                              [(drop 2 seq) []] (range 0 child-nodes-count))]
    [(drop metadata-count post-child-seq) (into [] (concat tree all-children (take metadata-count post-child-seq)))]))

(defn extract-metadata [seq]
  (second (parse-seq [seq []])))

(comment
  (->> input
       (extract-metadata)
       (flatten)
       (reduce +)
       (println "Sum of metadata is")))
; Sum of metadata is 47464

; --- Part Two ---
; The second check is slightly more complicated: you need to find the value of the root node.
; The value of a node depends on whether it has child nodes.
;
; If a node has no child nodes, its value is the sum of its metadata entries. So, the value of node B is 10+11+12=33, and the value of node D is 99.
;
; However, if a node does have child nodes, the metadata entries become indexes which refer to those child nodes. A metadata entry of 1 refers to the first child node, 2 to the second, 3 to the third, and so on. The value of this node is the sum of the values of the child nodes referenced by the metadata entries. If a referenced child node does not exist, that reference is skipped. A child node can be referenced multiple time and counts each time it is referenced. A metadata entry of 0 does not refer to any child node.
;
; What is the value of the root node?

(defn calculate-value [meta-seq]
  (let [children (into [] (take-while sequential? meta-seq))
        indices (drop-while sequential? meta-seq)]
    (if (empty? children)
        (reduce + indices)
        (reduce (fn [sum idx]
                  (if (nil? (get children (dec idx)))
                      sum
                      (+ sum (calculate-value (get children (dec idx))))))
                0 indices))))

(comment
  (->> input
       (extract-metadata)
       (calculate-value)
       (println "Root node value is")))
; Root node value is 23054
