(ns aoc2018.utils)

(defn string->strings [strs]
  (->>
    strs
    (clojure.string/split-lines)
    (map #(clojure.string/trim %))))

(defn s->i [s]
  (read-string s))

(defn resource->strings [input-file]
  (->>
      (clojure.java.io/resource input-file)
      (slurp)
      (string->strings)))

(defn resource->ints [input-file]
  (->>
      (clojure.java.io/resource input-file)
      (slurp)
      (#(clojure.string/split % #"\ "))
      (map s->i)))