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

(defn- reverse-subvec
  [v start n]
  (loop [nn (quot n 2)
         vv v
         from start
         to (-> n (+ start) dec (mod (count v)))]
    (if (pos? nn)
      (recur
        (dec nn)
        (assoc! vv from (get v to) to (get v from))
        (mod (inc from) (count v))
        (mod (dec to) (count v)))
      vv)))

(defn calculate-hash
  [n moves]
  (loop [moves moves
         inc-size 0
         vs (transient (into [] (range n)))
         idx 0]
    (if-some [[m & ms] (seq moves)]
      (recur
        ms
        (mod (inc inc-size) n)
        (reverse-subvec vs idx m)
        (mod (+ m inc-size idx) n))
      (persistent! vs))))

(defn day-10a-solution
  [input]
  (->> input
       clojure.string/trim
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (calculate-hash 256)
       (take 2)
       (reduce *)))

(defn- hash-str
  [in-str]
  (->> (concat (map int in-str) [17 31 73 47 23])
       (repeat 64)
       (apply concat)
       (calculate-hash 256)
       (eduction
         (comp
           (partition-all 16)
           (map (partial apply bit-xor))))))

(defn day-10b-solution
  [input]
  (->> input
       clojure.string/trim
       hash-str
       (map #(format "%02x" %))
       (apply str)))

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
     :rng rng
     :cycle-len (- (* rng 2) 2)}))

(defn- caught?
  [d {:keys [cycle-len layer]}]
  (-> layer (+ d) (mod cycle-len) zero?))

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

(defn- test-bit
  [byt mask]
  (-> byt (bit-and mask) zero? not))

(defn- bits
  [byt]
  (cond-> []
    (test-bit byt 1) (conj 0)
    (test-bit byt 2) (conj 1)
    (test-bit byt 4) (conj 2)
    (test-bit byt 8) (conj 3)
    (test-bit byt 16) (conj 4)
    (test-bit byt 32) (conj 5)
    (test-bit byt 64) (conj 6)
    (test-bit byt 128) (conj 7)))

(defn day-14a-solution
  [input]
  (->> (range 128)
       (partition-all 32)
       (pmap (partial
               transduce
               (comp
                 (map (partial format "%s-%d" input))
                 (map hash-str)
                 cat
                 (map #(Integer/bitCount %)))
               +))
       (reduce +)))

(defn- calc-bit-columns
  [byts]
  (eduction
    (comp
      (map bits)
      (map-indexed (fn [i bs] (map #(+ (* 8 i) %) bs)))
      cat)
    byts))

(defn- adjacent-coords
  [[a b]]
  #{[(inc a) b]
    [(dec a) b]
    [a (inc b)]
    [a (dec b)]})

(defn- remove-a-region
  [coords]
  (loop [coords coords
         co-q #{(first coords)}]
    (let [hits (clojure.set/intersection coords co-q)]
      (if (seq hits)
        (recur
          (clojure.set/difference coords co-q)
          (transduce (map adjacent-coords) clojure.set/union hits))
        coords))))

(defn day-14b-solution
  [input]
  (loop [c 0
         coords (->> (range 128)
                     (partition-all 32)
                     (pmap (partial
                             into []
                             (comp
                               (map (partial format "%s-%d" input))
                               (map hash-str)
                               (map calc-bit-columns))))
                     (into #{}
                           (comp cat
                                 (map-indexed (fn [i cs] (map (partial vector i) cs)))
                                 cat)))]
    (if-not (empty? coords)
      (recur (inc c) (remove-a-region coords))
      c)))

(defn- next-a
  [a]
  (-> a (* 16807) (mod 2147483647)))

(defn- next-b
  [b]
  (-> b (* 48271) (mod 2147483647)))

(defn- lower-16-bits-match?
  [a b]
  (->> (bit-xor a b)
       (bit-and 65535)
       zero?))

(defn day-15a-solution
  [init-a init-b]
  (loop [n (int 4e7)
         av init-a
         bv init-b
         matches 0]
    (if (pos? n)
      (recur
        (dec n)
        (next-a av)
        (next-b bv)
        (if-not (lower-16-bits-match? av bv)
          matches
          (inc matches)))
      matches)))

(defn- next-a-15b
  [a]
  (loop [a (next-a a)]
    (if-not (zero? (bit-and a 3))
      (recur (next-a a))
      a)))

(defn- next-b-15b
  [b]
  (loop [b (next-b b)]
    (if-not (zero? (bit-and b 7))
      (recur (next-b b))
      b)))

(defn day-15b-solution
  [init-a init-b]
  (loop [n (int 5e6)
         av (next-a-15b init-a)
         bv (next-b-15b init-b)
         matches 0]
    (if (pos? n)
      (recur
        (dec n)
        (next-a-15b av)
        (next-b-15b bv)
        (if-not (lower-16-bits-match? av bv)
          matches
          (inc matches)))
      matches)))

(defn- shift-position-mapper
  [n shift-amount]
  (into {}
        (for [i (range n)]
          [i (mod (+ i shift-amount) n)])))

(defn- swap-mapper
  [i j]
  {i j
   j i})

(defn- compile-day-16-mapper
  [n instruction]
  (or (if-let [[_ shift-amount] (re-matches #"s(\d+)" instruction)]
        (shift-position-mapper n (Integer/parseInt shift-amount)))
      (if-let [[_ from to] (re-matches #"x(\d+)/(\d+)" instruction)]
        (swap-mapper (Integer/parseInt from) (Integer/parseInt to)))
      (if-let [[_ from to] (re-matches #"p([a-z])/([a-z])" instruction)]
        (swap-mapper (first from) (first to)))
      (throw (format "Invalid instruction: %s" instruction))))

(defn- merge-day-16-mapper
  [acc mapper]
  (into {}
        (map (fn [[k v]] [k (get mapper v v)]))
        acc))

(defn- identity-mapper
  [vs]
  (into {}
        (map (juxt identity identity))
        (concat vs (-> vs count range))))

(defn- parse-day-16-instructions
  [vs instructions]
  (->> instructions
       (re-seq #"[^,\s]+")
       (transduce
         (map (partial compile-day-16-mapper (count vs)))
         (completing merge-day-16-mapper)
         (identity-mapper vs))))

(defn- apply-day-16-mapper
  [in-str mapper]
  (->> in-str
       (map-indexed (fn [i v] [(get mapper i i) (get mapper v v)]))
       (sort-by first)
       (map second)
       (apply str)))

(defn- merge-day-16-mapper-n-times
  [n mapper]
  (cond
    (<= n 1) mapper
    (even? n) (recur (quot n 2) (merge-day-16-mapper mapper mapper))
    :else (->> (merge-day-16-mapper mapper mapper)
               (merge-day-16-mapper-n-times (quot n 2))
               (merge-day-16-mapper mapper))))

(defn day-16-solution-impl
  [start-pos n-iterations instructions]
  (->> instructions
       (parse-day-16-instructions start-pos)
       (merge-day-16-mapper-n-times n-iterations)
       (apply-day-16-mapper start-pos)))

(defn day-16a-solution
  [input]
  (day-16-solution-impl "abcdefghijklmnop" 1 input))

(defn day-16b-solution
  [input]
  (day-16-solution-impl "abcdefghijklmnop" (int 1e9) input))

(defn day-17a-solution
  [input]
  (loop [n 1
         buf [0]]
    (if (<= n 2017)
      (recur
        (inc n)
        (into [n]
              (comp
                (drop (inc (mod input n)))
                (take n))
              (concat buf buf)))
      (second buf))))

(defn day-17b-solution
  [input]
  (loop [n 1
         pos 0
         v nil]
    (if (<= n (int 5e7))
      (let [new-pos (inc (mod (+ input pos) n))]
        (recur
          (inc n)
          new-pos
          (if-not (= 1 new-pos) v n)))
      v)))
