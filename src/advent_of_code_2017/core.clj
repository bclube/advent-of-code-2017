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

(defn- compute-cell-value
  [val-map [x y]]
  (transduce
    (map (fn [[xx yy]] (get val-map [(+ x xx) (+ y yy)] 0)))
    +
    [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]))

(defn- update-bounds
  [bounds [x y]]
  (-> bounds
      (update :max-x max x)
      (update :min-x min x)
      (update :max-y max y)
      (update :min-y min y)))

(defn day-3b-solution
  [v]
  (loop [[x y] [0 0]
         value-map {[0 0] 1}
         bounds {:max-x 0 :max-y 0 :min-x 0 :min-y 0}
         [[dx dy] & dirs :as all-dirs] (cycle [[1 0] [0 1] [-1 0] [0 -1]])]
    (let [new-loc [(+ x dx) (+ y dy)]
          new-val (compute-cell-value value-map new-loc)
          new-value-map (assoc value-map new-loc new-val)
          new-bounds (update-bounds bounds new-loc)
          new-dirs (if (= bounds new-bounds) all-dirs dirs)]
      (if (> new-val v)
        new-val
        (recur new-loc new-value-map new-bounds new-dirs)))))

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

(defn- index-of-max
  [vs]
  (if-not (seq vs)
    -1
    (let [mx (apply max vs)] ; TODO: room to improve efficiency, here (still O(n), though ;).
      (->> vs
           (map-indexed vector)
           (drop-while #(->> % second (not= mx)))
           ffirst))))

(defn- redistribute
  [blocks]
  (let [i-max (index-of-max blocks)
        v (get blocks i-max)
        q (quot v (count blocks))]
    (loop [i (inc i-max)
           rc (rem v (count blocks))
           bs (assoc blocks i-max q)]
      (let [ii (mod i (count blocks))]
        (if (= ii i-max)
          bs
          (let [amount (if (pos? rc) (inc q) q)]
            (recur (inc i) (dec rc) (update bs ii (partial + amount)))))))))

(defn- day-6-solution-impl
  [vs]
  (loop [c 1
         seen {}
         blocks vs]
    (let [rblocks (redistribute blocks)]
      (if (seen rblocks)
        [c (get seen rblocks)]
        (recur (inc c) (assoc seen rblocks c) rblocks)))))

(defn- day-6-solution
  [input f]
  (->> input
       parse-ints
       (into [])
       day-6-solution-impl
       f))

(defn day-6a-solution
  [input]
  (day-6-solution input (fn [[c _]] c)))

(defn day-6b-solution
  [input]
  (day-6-solution input (fn [[c1 c2]] (- c1 c2))))

(defn- parse-program-info
  [prog-spec]
  {:prog (->> prog-spec (re-find #"^\s*(\w+)") second)
   :weight (->> prog-spec (re-find #"^.*\((\d+)\)") second Integer/parseInt)
   :children (->> prog-spec (re-find #"^.*\)(.*)$") second (re-seq #"\w+") (into #{}))})

(defn day-7a-solution
  [input]
  (let [progs (->> input clojure.string/split-lines (map parse-program-info))
        all-children (->> progs (map :children) (apply clojure.set/union))]
    (some #(let [p (:prog %)] (if-not (all-children p) p)) progs)))

(defn- solve-7b
  [prog-lookup {:keys [weight children] :as prog}]
  (let [child-weights (map (comp #(solve-7b prog-lookup %) prog-lookup) children)]
    (if-let [result (some :result child-weights)]
      (assoc prog :result result) ; Leave as soon as result is found, possibly short-circuiting child-weights realization.
      (assoc prog
             :total-weight (transduce (map :total-weight) + weight child-weights)
             :result (let [[maj-w minority-w] (->> child-weights (map :total-weight) frequencies (sort-by second >) (map first))]
                       (if minority-w
                         (->> child-weights
                              (some #(if (-> % :total-weight (= minority-w)) (:weight %)))
                              (+ (- maj-w minority-w)))))))))

(defn day-7b-solution
  [input]
  (let [progs (->> input clojure.string/split-lines (map parse-program-info))
        lookup (into {} (map (juxt :prog identity)) progs)
        all-children (->> progs (map :children) (apply clojure.set/union))
        root (some #(if-not (-> % :prog all-children) %) progs)
        {:keys [result]} (solve-7b lookup root)]
    result))

(defn- compile-instr
  [line]
  (let [[target cmd amount _if cmp-l cmp-op cmp-r :as all] (re-seq #"\S+" line)
        amount (Integer/parseInt amount)
        cmd ({"inc" + "dec" -} cmd)
        cmp-op ({"<" < ">" > ">=" >= "==" = "<=" <= "!=" not=} cmp-op)
        cmp-r (Integer/parseInt cmp-r)]
    (fn [{:keys [current] :as registers}]
      (let [l (get current cmp-l 0)]
        (if (cmp-op l cmp-r)
          (let [old (get current target 0)
                nw (cmd old amount)]
            (-> registers
                (update :max-reg #(if % (max % nw) nw))
                (assoc-in [:current target] nw)))
          registers)))))

(defn day-8a-solution
  [input]
  (->> input
       clojure.string/split-lines
       (map compile-instr)
       (reduce
         (fn [regs instr] (instr regs))
         {:current {}})
       :current
       vals
       (apply max)))

(defn day-8b-solution
  [input]
  (->> input
       clojure.string/split-lines
       (map compile-instr)
       (reduce
         (fn [regs instr] (instr regs))
         {:current {}})
       :max-reg))

(defn day-9a-solution
  [input]
  (loop [score 0
         level 0
         css (-> input
                 clojure.string/trim
                 (clojure.string/replace #"!." "")
                 (clojure.string/replace #"\<.*?\>" "")
                 (clojure.string/replace #"," ""))]
    (if-let [[c & cs] (seq css)]
      (case c
        \{ (recur score (inc level) cs)
        \} (recur (+ score level) (dec level) cs))
      score)))

(defn day-9b-solution
  [input]
  (-> input
      clojure.string/trim
      (clojure.string/replace #"!." "")
      (->>
        (re-seq #"\<.*?\>")
        (transduce
          (comp
            (map count)
            (map #(- % 2)))
          +))))

(defn day-10a-solution-impl
  [n moves]
  (loop [moves moves
         inc-size 0
         vs (range n)
         idx 0]
    (if-some [[m & ms] (seq moves)]
      (let [i (+ m inc-size)
            [h t] (split-at m vs)
            new-vs (concat (reverse h) t)]
        (recur
          ms
          (mod (inc inc-size) n)
          (->> new-vs cycle (drop i) (take n) (into []))
          (mod (+ i idx) n)))
      (->> vs
           cycle
           (drop (- n idx))
           (take n)))))

(defn day-10a-solution
  [input]
  (->> input
       clojure.string/trim
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (day-10a-solution-impl 256)
       (take 2)
       (reduce *)))

(defn day-10b-solution
  [input]
  (-> input
      (->>
        clojure.string/trim
        (map int))
      (concat [17 31 73 47 23])
      (->>
        (repeat 64)
        (apply concat)
        (day-10a-solution-impl 256)
        (partition-all 16)
        (map #(apply bit-xor %))
        (map #(format "%02x" %))
        (apply str))))

(defn- cancel-dirs
  [dirs d d']
  (let [mn (min (get dirs d 0)
                (get dirs d' 0))]
    (if (zero? mn)
      dirs
      (-> dirs
          (update d - mn)
          (update d' - mn)))))

(defn- collapse-adjacent-dirs
  [dirs dr dm dl]
  (let [mn (min (get dirs dr 0)
                (get dirs dl 0))]
    (if (zero? mn)
      dirs
      (-> dirs
          (update dr - mn)
          (update dl - mn)
          (update dm (fnil + 0) mn)))))

(let [dirs ["n" "ne" "se" "s" "sw" "nw"]
      oppo-dirs (->> dirs (split-at (/ (count dirs) 2)) (apply map vector))
      adj-dirs (->> dirs cycle (partition 3 1) (take (count dirs)))]
  (defn- cancel-opposites
    [ds]
    (reduce (partial apply cancel-dirs) ds oppo-dirs))
  (defn- collapse-adjacent
    [ds]
    (reduce (partial apply collapse-adjacent-dirs) ds adj-dirs)))

(defn- find-shortest-path
  [ds]
  (loop [ds ds]
    (let [ds' (-> ds cancel-opposites collapse-adjacent)]
      (if (not= ds ds')
        (recur ds')
        ds))))

(defn day-11a-solution
  [input]
  (->> input
       (re-seq #"\w+")
       frequencies
       find-shortest-path
       (transduce (map second) +)))

(defn- add-dir
  [dirs d]
  (-> dirs
      (update d (fnil inc 0))
      find-shortest-path))

(defn day-11b-solution
  [input]
  (->> input
       (re-seq #"\w+")
       (reductions add-dir {})
       (map #(transduce (map second) + %))
       (reduce max)))

(defn- parse-day-12-graph-node
  [line]
  (let [[from to :as all] (clojure.string/split line #" \<\-\> ")]
    {:from (->> from clojure.string/trim Integer/parseInt)
     :to (->> to (re-seq #"\d+") (into #{} (map #(Integer/parseInt %))))}))

(defn- reachable-from
  [i mp]
  (transduce
    (map #(reachable-from % (dissoc mp i)))
    clojure.set/union
    #{i}
    (get mp i [])))

(defn- parse-day-12-graph
  [input]
  (->> input
       clojure.string/split-lines
       (into {}
             (comp
               (map parse-day-12-graph-node)
               (map (juxt :from :to))))))

(defn day-12a-solution
  [input]
  (->> input
       parse-day-12-graph
       (reachable-from 0)
       count))

(defn day-12b-solution
  [input]
  (loop [mp (parse-day-12-graph input)
         c 0]
    (if-let [[i _] (first mp)]
      (let [ch (reachable-from i mp)]
        (recur (apply dissoc mp ch) (inc c)))
      c)))

(defn- parse-day-13-layer
  [line]
  (let [[layer rng] (->> line
                         (re-seq #"\d+")
                         (map #(Integer/parseInt %)))]
    {:layer layer
     :rng rng}))

(defn- caught?
  [d {:keys [layer rng]}]
  (zero?
    (mod (+ layer d)
         (- (* rng 2) 2))))

(defn day-13a-solution
  [input]
  (->> input
       clojure.string/split-lines
       (transduce
         (comp
           (map parse-day-13-layer)
           (filter (partial caught? 0))
           (map #(* (:layer %) (:rng %))))
         +)))

(defn day-13b-solution
  [input]
  (let [lines (->> input clojure.string/split-lines (map parse-day-13-layer) (sort-by :rng))]
    ; Maybe there's a non-brute-force way to solve this?
    (loop [d 0]
      (if (some (partial caught? d) lines)
        (recur (inc d))
        d))))
