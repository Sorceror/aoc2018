(ns aoc2018.utils)

(defn resource->strings [input-file]
  (->>
    (clojure.java.io/resource input-file)
    (slurp)
    (clojure.string/split-lines)
    (map #(clojure.string/trim %))))

(defn s->i [s]
  (read-string s))