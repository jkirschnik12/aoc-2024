(ns aoc-2024.day2
  (:require
   [aoc-2024.utils :as u]
   [clojure.string :as str]
   [clojure.edn :as edn]))

(defn parse
  []
  (->> (u/read-input 2)
       (map #(str/split % #"\s+"))
       (map #(map edn/read-string %))))

(defn check-report
  [r]
  (loop [[prev cur & rem] r
         ord nil]
    (if-not cur
      true
      (let [ord (or ord (if (< prev cur) < >))]
        (if (and (ord prev cur)
                 (<= (abs (- prev cur)) 3))
          (recur (cons cur rem) ord)
          false)))))

(defn solve-1
  []
  (->> (parse)
       (map check-report)
       (filter identity)
       count))

(defn check-report-2
  [r & {:keys [f] :or {f 0}}]
  (loop [[prev cur & rem] r
         ord nil
         f f]
    (if-not cur
      true
      (let [ord (or ord (if (< prev cur) < >))]
        (if (and (ord prev cur)
                 (<= (abs (- prev cur)) 3))
          (recur (cons cur rem) ord f)
          (if (zero? f)
            ;; definitely brute force
            (some #(check-report-2
                    (keep-indexed
                     (fn [idx v]
                       (when (not= idx %)
                         v)) r) :f 1) (range 0 (count r)))
            false))))))

(defn solve-2
  []
  (->> (parse)
       (map check-report-2)
       (filter identity)
       count))

(comment (solve-1) 
  (solve-2))

