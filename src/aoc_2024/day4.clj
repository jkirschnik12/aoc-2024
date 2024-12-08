(ns aoc-2024.day4
  (:require
   [aoc-2024.utils :as u]
   [clojure.edn :as edn]))

(def sample
  ["MMMSXXMASM"
   "MSAMXMSMSA"
   "AMXSXMAAMM"
   "MSAMASMSMX"
   "XMASAMXAMM"
   "XXAMMXXAMA"
   "SMSMSASXSS"
   "SAXAMASAAA"
   "MAMMMXMMMM"
   "MXMXAXMASX"])

(defn parse
  []
  (u/read-input 4))

(defn inc-2d
  [x y rows columns]
  (if (>= (inc x) columns)
    (if-not (>= (inc y) rows)
      [0 (inc y)]
      [nil nil])
    [(inc x) y]))

(def m "XMAS")

(defn oob?
  [x y in]
  (or (neg? x)
      (neg? y)
      (>= x (count (first in)))
      (>= y (count in))))

(defn xmas?
  [[x y] in [x-fn y-fn] pos]
  (let [expected (get m pos)]
    (if-not expected
      true
      (if (or (nil? expected)
              (oob? x y in)
              (not= (get-in in [y x]) expected))
        false
        (xmas? [(x-fn x) (y-fn y)] in [x-fn y-fn] (inc pos))))))

(defn traverse
  [[x y] in]
  (count (filter (fn [mv-fns]
                   (xmas? [x y] in mv-fns 0))
                 [[inc identity]
                  [inc inc]
                  [inc dec]
                  [identity inc]
                  [dec identity]
                  [dec dec]
                  [dec inc]
                  [identity dec]])))

(defn solve-1
  [in]
  (let [rows (count in)
        columns (count (first in))]
    (loop [[x y] [0 0]
           result 0]
      (if (or (nil? x) (nil? y))
        result
        (if (= "X" (str (get-in in [y x])))
          (let [times (traverse [x y] in)]
            (recur (inc-2d x y rows columns)
                   (+ result times)))
          (recur (inc-2d x y rows columns) result))))))

(defn x-mas?
  [coord-1 coord-2 in]
  (println [coord-1 coord-2])
  (= (into #{} (keep (fn [[x y]]
                       (when-not (oob? x y in)
                         (str (get-in in [y x])))) [coord-1 coord-2]))
     #{"M" "S"}))

(defn traverse-2
  [x y in]
  (every? (fn [[[x-fn y-fn] [x-fn2 y-fn2]]]
            (x-mas? [(x-fn x) (y-fn y)]
                    [(x-fn2 x) (y-fn2 y)] in))
          [[[inc inc] [dec dec]]
           [[inc dec] [dec inc]]]))

(defn solve-2
  [in]
  (let [rows    (count in)
        columns (count (first in))]
    (loop [[x y]  [0 0]
           result 0]
      (if (or (nil? x) (nil? y))
        result
        (if (= "A" (str (get-in in [y x])))
          (recur (inc-2d x y rows columns)
                 (if (traverse-2 x y in)
                   (inc result)
                   result))
          (recur (inc-2d x y rows columns) result))))))

(comment
  (solve-1 (parse))
  (solve-2 (parse))1925
  (get-in sample [0 0]))

