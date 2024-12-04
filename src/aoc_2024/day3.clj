(ns aoc-2024.day3
  (:require
   [aoc-2024.utils :as u]
   [clojure.edn :as edn]))

(defn parse
  []
  (u/read-input 3 :lines false))

(defn execute-mul
  [mul-str]
  (->> (re-seq #"\d{1,3}" mul-str)
       (map edn/read-string)
       (apply *)))

(defn solve-1
  []
  (->> (parse)
       (re-seq #"mul\(\d{1,3},\d{1,3}\)")
       (map execute-mul)
       (apply +)))

(defn solve-2
  []
  (->> (parse)
       (re-seq #"mul\(\d{1,3},\d{1,3}\)|do\(\)|don\'t\(\)")
       (reduce (fn [{:keys [status] :as m} val]
                 (cond
                   (= val "do()")
                   (assoc m :status true)
                   (= val "don't()")
                   (assoc m :status false)
                   :else
                   (if status
                     (update m :ans + (execute-mul val))
                     m)))
               {:status true :ans 0})
       :ans))

(comment
  (solve-1)
  (solve-2))

