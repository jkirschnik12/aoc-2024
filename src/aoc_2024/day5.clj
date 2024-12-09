(ns aoc-2024.day5
  (:require
   [aoc-2024.utils :as u]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.set :as set]))

(def sample
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn parse
  []
  (let [[rules _ orderings] (->> (u/read-input 5)
                                 (partition-by str/blank?))]
    {:rules (mapv (fn [rule]
                    (->> (str/split rule #"\|")
                         (mapv edn/read-string))) rules)
     :lists (mapv (fn [lst]
                    (->> (str/split lst #",")
                         (mapv edn/read-string))) orderings)}))

(defn construct-rules-thingy
  [rules]
  (->> rules
       (reduce (fn [acc [pre post]]
                 (-> acc
                     (update-in [:before pre] (fn [c]
                                                (if (seq c)
                                                  (conj c post)
                                                  #{post})))
                     (update-in [:after post] (fn [c]
                                                (if (seq c)
                                                  (conj c pre)
                                                  #{pre}))))) {:before {} :after {}})))

(defn list-sorted?
  [rules lst]
  (loop [before #{}
         cur    (first lst)
         after  (rest lst)]
    (let [bf-rule   (get-in rules [:before cur])
          af-rule   (get-in rules [:after cur])
          after-set (set after)]
      (if-not cur
        true
        (if (or (seq (set/intersection bf-rule before))
                (seq (set/intersection af-rule after-set)))
          false
          (recur (conj before cur) (first after) (rest after)))))))

(defn solve-1
  [in]
  (let [{:keys [rules lists]} in]
    (->> (keep (fn [lst]
             (when (list-sorted? (construct-rules-thingy rules) lst)
               lst)) lists)
         (reduce (fn [acc lst]
                   (let [len (count lst)
                         mid-idx (quot len 2)]
                     (+ acc (nth lst mid-idx)))) 0))))

(defn sort-lists-somehow
  [{:keys [before after]} lst]
  (reduce (fn [sorted el]) [] lst))

(defn solve-2
  [in]
  (let [{:keys [rules lists]} in
        rules (construct-rules-thingy rules)]
    (->> (keep (fn [lst]
             (when-not (list-sorted? rules lst)
               lst)) lists)
         (map (partial sort-lists-somehow rules))
         (reduce (fn [acc lst]
                   (let [len (count lst)
                         mid-idx (quot len 2)]
                     (+ acc (nth lst mid-idx)))) 0))))

(comment
  (solve-1 (parse))
  (solve-2 (parse))
)

