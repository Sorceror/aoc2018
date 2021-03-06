(ns aoc2018.utils)

(defn string->strings [trim-opt strings]
  (->>
    strings
    (clojure.string/split-lines)
    (#(if (true? trim-opt)
        (map (fn [s] clojure.string/trim s) %)
        %))))

(defn s->i [s]
  (read-string s))

(defn resource->single-string
  ([input-file] (resource->single-string input-file false))
  ([input-file trim-opt]
   (->> (clojure.java.io/resource input-file)
        (slurp)
        (#(if (true? trim-opt) (clojure.string/trim %) %)))))

(defn resource->strings
  ([input-file] (resource->strings input-file true))
  ([input-file trim-opt]
   (->>
       (resource->single-string input-file)
       (string->strings trim-opt))))

(defn resource->ints [input-file]
  (->>
      (clojure.java.io/resource input-file)
      (slurp)
      (#(clojure.string/split % #"\ "))
      (map s->i)))