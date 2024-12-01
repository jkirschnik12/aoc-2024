(ns aoc-2024.utils
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn read-input
  [d & {:keys [lines] :or {lines true}}]
  (cond-> (slurp (io/resource (str "day" d ".txt")))
    lines
    (str/split-lines)))
