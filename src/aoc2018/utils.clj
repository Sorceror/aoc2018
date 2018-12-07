(ns aoc2018.utils)

(defn string->strings [strs]
  (->>
    strs
    (clojure.string/split-lines)
    (map #(clojure.string/trim %))))

(defn resource->strings [input-file]
  (->>
      (clojure.java.io/resource input-file)
      (slurp)
      (string->strings)))

(defn s->i [s]
  (read-string s))