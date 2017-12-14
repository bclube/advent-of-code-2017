(ns advent-of-code-2017.core
  (:require [clj-http.client :as client]
            [environ.core :refer [env]]))

(def cookie
  (:cookie env))

(defn- input-url
  [day]
  (format "https://adventofcode.com/2017/day/%d/input" day))

(defn- char-to-int
  [c]
  (-> c str Integer/parseInt))

(defn get-input!
  [day]
  (-> day
      input-url
      (client/get {:headers {"Cookie" cookie}})
      :body))

(defn- to-digit-seq
  [s]
  (->> s
       clojure.string/trim-newline
       (map char-to-int)))

(defn day-1-solution
  [input]
  (let [digits (to-digit-seq input)
        digits-circ (concat digits [(first digits)])]
    (- (reduce + digits-circ)
       (reduce + (dedupe digits-circ)))))

(defn day-1b-solution
  [input]
  (let [digits (to-digit-seq input)
        [as bs] (split-at (/ (count digits) 2) digits)
        sums (map (fn [a b] (if (= a b) (+ a b) 0)) as bs)]
    (reduce + sums)))

(defn- parse-ints
  [int-str]
  (->> int-str (re-seq #"\d+") (map #(Integer/parseInt %))))

(defn- calc-row-diff
  [int-row]
  (let [mn (apply min int-row)
        mx (apply max int-row)]
    (- mx mn)))

(defn day-2a-solution
  [input]
  (->> input
       clojure.string/split-lines
       (transduce
         (comp
           (map parse-ints)
           (map calc-row-diff))
         +)))

(defn- solve-2b-row
  [input]
  (loop [vss (sort > input)]
    (if-let [[v & vs] (seq vss)]
      (or (some #(if (zero? (rem v %)) (/ v %)) vs)
          (recur vs)))))

(defn day-2b-solution
  [input]
  (->> input
       clojure.string/split-lines
       (transduce
         (comp
           (map parse-ints)
           (map solve-2b-row))
         +)))

(defn- valid-day-4-passphrase?
  [passphrase xform-fn]
  (loop [seen #{}
         words (re-seq #"\w+" passphrase)]
    (if-some [[w & ws] (seq words)]
      (let [xw (xform-fn w)]
        (and (not (seen xw))
             (recur (conj seen xw) ws)))
      true)))

(defn valid-day-4a-passphrase?
  [passphrase]
  (valid-day-4-passphrase? passphrase identity))

(defn valid-day-4b-passphrase?
  [passphrase]
  (valid-day-4-passphrase? passphrase (comp clojure.string/join sort)))

(defn- day-4-solution
  [input pred]
  (->> input
       clojure.string/split-lines
       (filter pred)
       count))

(defn day-4a-solution
  [input]
  (day-4-solution input valid-day-4a-passphrase?))

(defn day-4b-solution
  [input]
  (day-4-solution input valid-day-4b-passphrase?))

(defn- iterate-5-steps
  [maze xform-fn]
  (loop [c 0
         i 0
         m maze]
    (if-some [v (get m i)]
      (recur (inc c) (+ i v) (assoc m i (xform-fn v)))
      c)))

(defn- iterate-5a-steps
  [maze]
  (iterate-5-steps maze inc))

(defn- iterate-5b-steps
  [maze]
  (iterate-5-steps maze #(if (< % 3) (inc %) (dec %))))

(defn- parse-day-5-input
  [input]
  (->> input
       clojure.string/split-lines
       (into [] (map #(Integer/parseInt %)))))

(defn day-5a-solution
  [input]
  (->> input
       parse-day-5-input
       iterate-5a-steps))

(defn day-5b-solution
  [input]
  (->> input
       parse-day-5-input
       iterate-5b-steps))
