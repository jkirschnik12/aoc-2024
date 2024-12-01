(ns aoc-2024.day1
  (:require
   [aoc-2024.utils :as u]
   [clojure.string :as str]
   [clojure.edn :as edn]))

(defn parse
  []
  (->> (u/read-input 1)
       (map #(str/split % #"\s+"))
       (reduce (fn [[l1 l2] [v1 v2]]
                 [(conj l1 (edn/read-string v1))
                  (conj l2 (edn/read-string v2))]) [])))

(defn solve-1
  []
  (let [[l1 l2] (map sort (parse))]
    (->> (map (fn [v1 v2]
                (abs (- v1 v2)))  l1 l2)
         (apply +))))

(defn solve-2
  []
  (let [[l1 l2] (parse)
        freq (frequencies l2)]
    (->> (map #(* % (get freq % 0)) l1)
         (apply +))))

(comment
  (solve-1)
  (solve-2))

