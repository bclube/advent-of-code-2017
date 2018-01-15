(ns advent-of-code-2017.core
  (:require [clj-http.client :as client]))

(def cookie
  (System/getenv "cookie"))

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
  (condp re-matches instruction
    #"s(\d+)" :>> (fn [[_ shift-amount]] (shift-position-mapper n (Integer/parseInt shift-amount)))
    #"x(\d+)/(\d+)" :>> (fn [[_ from to]] (swap-mapper (Integer/parseInt from) (Integer/parseInt to)))
    #"p([a-z])/([a-z])" :>> (fn [[_ from to]] (swap-mapper (first from) (first to)))))

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

(defn- get-reg
  [reg state]
  (get-in state [:registers reg] 0))

(defn send-value
  [value state]
  (-> state
      (update :send-count (fnil inc 0))
      (assoc :to-send value)))

(defn send-register
  [reg state]
  (send-value (get-reg reg state) state))

(defn- register-value-op
  [reg value update-fn state]
  (update-in state [:registers reg] (fnil update-fn 0) value))

(defn- registers-op
  [reg-to reg-from update-fn state]
  (register-value-op reg-to (get-reg reg-from state) update-fn state))

(defn receive-message
  [reg state]
  (if-let [msg (peek (:message-queue state))]
    (-> state
        (update :message-queue pop)
        (assoc :last-received msg)
        (assoc-in [:registers reg] msg))
    (assoc state :next-instruction (:current-instruction state))))

(defn jump-if-val
  [v distance if-fn state]
  (if (if-fn v)
    (assoc state :next-instruction (+ distance (get state :current-instruction)))
    state))

(defn jump-if-reg
  [reg distance if-fn state]
  (jump-if-val (get-reg reg state) distance if-fn state))

(defn jump-res-amount-if
  [reg-test reg-magnitude if-fn state]
  (jump-if-reg reg-test (get-reg reg-magnitude state) if-fn state))

(defn- parse-day-18-instruction
  [line]
  (condp re-matches (clojure.string/trim line)
    #"snd (-?\d+)" :>> (fn [[_ value]] (partial send-value (Integer/parseInt value)))

    #"(\w+) (\w)"
    :>> (fn [[_ op-name reg]]
          (let [reg (first reg)]
            (case op-name
              "rcv" (partial receive-message reg)
              "snd" (partial send-register reg))))

    #"(\w+) (-?\d+) (-?\d+)"
    :>> (fn [[_ op-name val-r val-l]]
          (let [val-r (Integer/parseInt val-r)
                val-l (Integer/parseInt val-l)]
            (case op-name
              "jgz" (partial jump-if-val val-r val-l pos?)
              "jnz" (partial jump-if-val val-r val-l (complement zero?)))))

    #"(\w+) (\w) (-?\d+)"
    :>> (fn [[_ op-name reg value]]
          (let [reg (first reg)
                value (Integer/parseInt value)]
            (case op-name
              "set" (partial register-value-op reg value (fn [_ v] v))
              "add" (partial register-value-op reg value +)
              "sub" (partial register-value-op reg value -)
              "mul" (comp
                      #(update % :mul-count (fnil inc 0))
                      (partial register-value-op reg value *))
              "mod" (partial register-value-op reg value mod)
              "jgz" (partial jump-if-reg reg value pos?)
              "jnz" (partial jump-if-reg reg value (complement zero?)))))

    #"(\w+) (\w) (\w)"
    :>> (fn [[_ op-name reg-to reg-from]]
          (let [reg-to (first reg-to)
                reg-from (first reg-from)]
            (case op-name
              "set" (partial registers-op reg-to reg-from (fn [_ v] v))
              "add" (partial registers-op reg-to reg-from +)
              "sub" (partial registers-op reg-to reg-from -)
              "mul" (comp
                      #(update % :mul-count (fnil inc 0))
                      (partial registers-op reg-to reg-from *))
              "mod" (partial registers-op reg-to reg-from mod)
              "jgz" (partial jump-res-amount-if reg-to reg-from pos?)
              "jnz" (partial jump-res-amount-if reg-to reg-from (complement zero?)))))))

(defn init-prog
  [p]
  {:registers {\p p}
   :next-instruction 0
   :message-queue clojure.lang.PersistentQueue/EMPTY})

(defn enqueue-message
  [prog value]
  (update prog :message-queue conj value))

(defn- advance-day-18-prog
  [instructions prog]
  (if-some [instr (->> prog :next-instruction (get instructions))]
    (-> prog
        (assoc :current-instruction (:next-instruction prog))
        (update :next-instruction inc)
        instr)
    (assoc prog :done? true)))

(defn day-18a-solution
  [input]
  (let [instructions (into []
                           (map parse-day-18-instruction)
                           (clojure.string/split-lines input))]
    (loop [prog (init-prog 0)]
      (cond
        (:last-received prog) (:last-received prog)
        (:done? prog) nil
        (:to-send prog) (recur (-> prog ; send message to self
                                   (dissoc :to-send)
                                   (enqueue-message (:to-send prog))))
        (= (:current-instruction prog)
           (:next-instruction prog)) (recur (update prog :next-instruction inc)) ; Blocked on empty message queue. Manually "unstick" it.
        :default (recur (advance-day-18-prog instructions prog))))))

(defn day-18b-solution
  [input]
  (let [instructions (into []
                           (map parse-day-18-instruction)
                           (clojure.string/split-lines input))]
    (loop [prog-0 (init-prog 0)
           prog-1 (init-prog 1)]
      (cond
        (:done? prog-1) (get prog-1 :send-count 0)
        (:to-send prog-0) (recur (dissoc prog-0 :to-send) ; send message from prog-0 to prog-1
                                 (enqueue-message prog-1 (:to-send prog-0)))
        (:to-send prog-1) (recur (enqueue-message prog-0 (:to-send prog-1)) ; send message from prog-1 to prog-0
                                 (dissoc prog-1 :to-send))
        :default (let [new-prog-0 (advance-day-18-prog instructions prog-0)
                       new-prog-1 (advance-day-18-prog instructions prog-1)]
                   (if (or (not= new-prog-0 prog-0)
                           (not= new-prog-1 prog-1))
                     (recur new-prog-0 new-prog-1)
                     (get prog-1 :send-count 0)))))))

(defn- get-map-char
  [mp [r c]]
  (-> mp
      (get r [])
      (get c \space)))

(defn- next-pos
  [[r c :as pos] [dr dc :as dir]]
  [(+ r dr) (+ c dc)])

(defn- turn
  [[r c :as pos] [dr dc :as dir] mp]
  (some #(if (->> % (next-pos pos) (get-map-char mp) (not= \space)) %)
        [[dc dr] [(- dc) (- dr)]]))

(defn- day-19-solution
  [input]
  (let [mp (->> input clojure.string/split-lines (into []))
        start-col (-> (get mp 0) (clojure.string/index-of \|))]
    (loop [step-count 1
           position [0 start-col]
           direction [1 0]
           letters []]
      (let [nxt (next-pos position direction)
            chr (get-map-char mp nxt)]
        (cond
          (re-matches #"[a-zA-Z]" (str chr)) (recur (inc step-count) nxt direction (conj letters chr))
          (= \+ chr) (recur (inc step-count) nxt (turn nxt direction mp) letters)
          (#{\| \-} chr) (recur (inc step-count) nxt direction letters)
          :else [step-count (apply str letters)])))))

(defn day-19a-solution
  [input]
  (let [[_ letters] (day-19-solution input)]
    letters))

(defn day-19b-solution
  [input]
  (let [[step-count _] (day-19-solution input)]
    step-count))

(defn- p-fn
  [p v a]
  (let [half-a (/ a 2.0)
        half-a-plus-v (+ v half-a)]
    (fn [t] (+ p
               (* t half-a-plus-v)
               (* t t half-a)))))

(defn- parse-day-20-particle
  [line]
  (let [[xp yp zp xv yv zv xa ya za] (->> line
                                          clojure.string/trim
                                          (re-matches #"p=<(.*),(.*),(.*)>, v=<(.*),(.*),(.*)>, a=<(.*),(.*),(.*)>")
                                          rest
                                          (map #(Integer/parseInt %)))
        x-fn (p-fn xp xv xa)
        y-fn (p-fn yp yv ya)
        z-fn (p-fn zp zv za)]
    (fn [t] [(x-fn t) (y-fn t) (z-fn t)])))

(defn- manhattan-distance
  [pos]
  (transduce
    (map #(Math/abs %))
    +
    pos))

(defn day-20a-solution
  [input]
  (->> input
       clojure.string/split-lines
       (eduction
         (comp
           (map parse-day-20-particle)
           (map #(% 1e12))
           (map manhattan-distance)
           (map-indexed vector)))
       (reduce (partial min-key second))
       first))

(defn day-20b-solution
  [input]
  (loop [t 0
         p-fns (->> input
                    clojure.string/split-lines
                    (map parse-day-20-particle)
                    (into []))]
    (if (and (< t (int 1e4))
             (-> p-fns count (> 1)))
      (recur (inc t)
             (->> p-fns
                  (group-by #(% t))
                  vals
                  (into []
                        (comp
                          (remove #(-> % count (> 1)))
                          cat))))
      (count p-fns))))

(defn- reverse-rows
  [pattern]
  [pattern
   (map clojure.string/reverse pattern)])

(defn- reverse-cols
  [pattern]
  [pattern
   (reverse pattern)])

(defn- transpose ; [row col] => [col row]
  [pattern]
  [pattern
   (apply map str pattern)])

(defn- pattern-lines
  [pattern]
  (clojure.string/split pattern #"/"))

(defn- parse-day-21-pattern
  [line]
  (let [[_ l-pattern r-pattern] (->> line
                                     clojure.string/trim
                                     (re-matches #"(.+) => (.+)"))
        l-pat (pattern-lines l-pattern)
        r-pat (pattern-lines r-pattern)]
    (eduction
      (comp
        (mapcat reverse-rows)
        (mapcat reverse-cols)
        (mapcat transpose)
        (map clojure.string/join)
        (map #(vector % r-pat)))
      [l-pat])))

(defn- parse-day-21-input-patterns
  [input]
  (->> input
       clojure.string/split-lines
       (into {}
             (mapcat parse-day-21-pattern))))

(defn- enhance-pattern
  [pattern pattern-map]
  (let [block-size (if (-> pattern count even?) 2 3)
        blocks-in-row (-> pattern count (quot block-size))]
    (into []
          (comp
            ; [1122 1122 3344 3344]
            (partition-all block-size)
            ; [[1122 1122] [3344 3344]]
            (mapcat (partial apply interleave))
            ; [\1 \1 \1 \1 \2 \2 \2 \2 \3 \3 \3 \3 \4 \4 \4 \4]
            (partition-all (* block-size block-size))
            ; [[\1 \1 \1 \1] [\2 \2 \2 \2] [\3 \3 \3 \3] [\4 \4 \4 \4]]
            (map clojure.string/join)
            ; [1111 2222 3333 4444]
            (map pattern-map)
            ; [[aaa aaa aaa] [bbb bbb bbb] [ccc ccc ccc] [ddd ddd ddd]]
            (partition-all blocks-in-row)
            ; [[[aaa aaa aaa] [bbb bbb bbb]] [[ccc ccc ccc] [ddd ddd ddd]]]
            (mapcat (partial apply interleave))
            ; [aaa bbb aaa bbb aaa bbb ccc ddd ccc ddd ccc ddd]
            (partition-all blocks-in-row)
            ; [[aaa bbb] [aaa bbb] [aaa bbb] [ccc ddd] [ccc ddd] [ccc ddd]]
            (map clojure.string/join)
            ; [aaabbb aaabbb aaabbb cccddd cccddd cccddd]
            )
          pattern)))

(defn- count-set-pixels
  [pattern]
  (transduce
    (comp
      (map (partial keep #{\#}))
      (map count))
    +
    pattern))

(defn day-21a-solution-impl
  [iterations input]
  (let [pattern-map (parse-day-21-input-patterns input)]
    (loop [i iterations
           pattern (pattern-lines ".#./..#/###")]
      (if (pos? i)
        (recur (dec i) (enhance-pattern pattern pattern-map))
        (count-set-pixels pattern)))))

(defn day-21a-solution
  [input]
  (day-21a-solution-impl 5 input))

(defn day-21b-solution
  [input]
  (day-21a-solution-impl 18 input))

(defn- turn-right
  [[r c]]
  [c (- r)])

(defn- turn-left
  [[r c]]
  [(- c) r])

(defn- reverse-dir
  [[r c]]
  [(- r) (- c)])

(defn day-22-solution-impl
  [steps grid inf-v turn-fn infect-fn]
  (let [lines (clojure.string/split-lines grid)
        infected-coords (into {}
                              (comp
                                (map clojure.string/trim)
                                (map-indexed (fn [r vs]
                                               (keep-indexed (fn [c v] (if (= \# v) [r c])) vs)))
                                cat
                                (map #(vector % inf-v)))
                              lines)
        middle (-> lines count (quot 2))]
    (loop [s steps
           infect-count 0
           dir [-1 0]
           loc [middle middle]
           grid infected-coords]
      (if (pos? s)
        (let [grid-val (get grid loc 0)
              new-dir (turn-fn grid-val dir)
              new-infect-state (infect-fn grid-val)
              new-grid (assoc grid loc new-infect-state)
              new-infect-count (if (not= inf-v new-infect-state) infect-count (inc infect-count))
              new-loc (next-pos new-dir loc)]
          (recur (dec s) new-infect-count new-dir new-loc new-grid))
        infect-count))))

(defn day-22a-solution-impl
  [steps grid]
  (day-22-solution-impl steps grid 1
                        (fn [i dir] (if (zero? i) (turn-left dir) (turn-right dir)))
                        #(-> % inc (mod 2))))

(defn day-22b-solution-impl
  [steps grid]
  (day-22-solution-impl steps grid 2
                        (fn [i dir] (case i
                                      0 (turn-left dir)
                                      1 dir
                                      2 (turn-right dir)
                                      3 (reverse-dir dir)))
                        #(-> % inc (mod 4))))

(defn day-22a-solution
  [input]
  (day-22a-solution-impl 10000 input))

(defn day-22b-solution
  [input]
  (day-22b-solution-impl 10000000 input))

(defn day-23a-solution
  [input]
  (let [instructions (into [] (map parse-day-18-instruction)
                           (clojure.string/split-lines input))]
    (loop [prog (init-prog 0)
           reg nil]
      (let [new-reg (get prog :registers)]
        (if-not (= reg new-reg) (prn new-reg))
        (if-not (:done? prog)
          (recur (advance-day-18-prog instructions prog) new-reg)
          (get prog :mul-count 0))))))

(defn- prime?
  [v]
  (->> (range 2 (-> v Math/sqrt Math/floor int inc))
       (not-any? #(->> % (mod v) zero?))))

(defn day-23b-solution
  "Manual translation of the assembly language program. Counts the number of
   non-prime values in the given range."
  []
  (let [b 106700
        c (+ b 17001)]
    (->> (range b c 17)
         (remove prime?)
         count)))

(defn- find-day-24-max
  [start-val inventory index max-fn]
  (transduce
    (comp
      (remove #(zero? (get inventory % 0)))
      (map (fn [[l r :as pair]]
             (let [[v len] (find-day-24-max
                             (if (= start-val l) r l)
                             (update inventory pair dec)
                             index
                             max-fn)]
               [(+ l r v) (inc len)]))))
    (completing max-fn)
    [0 0]
    (get index start-val [])))

(defn- parse-day-24-input
  [input]
  (let [pairs (->> input
                   (re-seq #"\d+")
                   (eduction
                     (comp
                       (map #(Integer/parseInt %))
                       (partition-all 2)
                       (map (partial apply vector)))))
        inventory (frequencies pairs)
        index (merge-with into
                          (group-by first pairs)
                          (group-by second pairs))]
    [inventory index]))

(defn- day-24-impl
  [input mx-fn]
  (let [[inventory index] (parse-day-24-input input)
        [mv _] (find-day-24-max 0 inventory index mx-fn)]
    mv))

(defn day-24a-solution
  [input]
  (day-24-impl input (fn [[v _] [v2 _]] [(max v v2) 0])))

(defn day-24b-solution
  [input]
  (day-24-impl input (fn [[v l] [v2 l2]]
                       (cond
                         (= l l2) [(max v v2) l]
                         (< l l2) [v2 l2]
                         :default [v l]))))

(let [write-0 disj
      write-1 conj
      move-r inc
      move-l dec]
  (def prog
    {\A [[write-1 move-r \B]
         [write-0 move-l \F]]
     \B [[write-0 move-r \C]
         [write-0 move-r \D]]
     \C [[write-1 move-l \D]
         [write-1 move-r \E]]
     \D [[write-0 move-l \E]
         [write-0 move-l \D]]
     \E [[write-0 move-r \A]
         [write-1 move-r \C]]
     \F [[write-1 move-l \A]
         [write-1 move-r \A]]}))

(defn day-25a-solution
  []
  (loop [n 12794428
         st \A
         i 0
         tp #{}]
    (if (pos? n)
      (let [v (if (contains? tp i) 1 0)
            [write-fn move-fn next-state] (get-in prog [st v])]
        (recur (dec n) next-state (move-fn i) (write-fn tp i)))
      (count tp))))
