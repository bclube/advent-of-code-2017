(ns advent-of-code-2017.core-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2017.core :refer :all]))

(deftest day-1-solution-test
  (is (= 3 (day-1-solution "1122")))
  (is (= 4 (day-1-solution "1111")))
  (is (= 0 (day-1-solution "1234")))
  (is (= 9 (day-1-solution "91212129"))))

(deftest day-1b-solution-test
  (is (= 6 (day-1b-solution "1212")))
  (is (= 0 (day-1b-solution "1221")))
  (is (= 4 (day-1b-solution "123425")))
  (is (= 12 (day-1b-solution "123123")))
  (is (= 4 (day-1b-solution "12131415"))))

(deftest day-2a-solution-test
  (is (= 18 (day-2a-solution "5 1 9 5\n7 5 3\n2 4 6 8")))
  (is (= 18 (day-2a-solution "15 11 19 15\n227 225 223\n3332 3334 3336 3338")))
  (is (= 18 (day-2a-solution "5\t1\t9\t5
                              7\t5\t3
                              2\t4\t6\t8"))))

(deftest day-2b-solution-test
  (is (= 9 (day-2b-solution "5 9 2 8\n9 4 7 3\n3 8 6 5")))
  (is (= 9 (day-2b-solution "21 91 20 80\n999 444 777 333\n3 8 6 5")))
  (is (= 9 (day-2b-solution "5\t9\t2\t8
                             9\t4\t7\t3
                             3\t8\t6\t5"))))

(deftest day-3b-solution-test
  (is (= 1 (day-3b-solution 0)))
  (is (= 2 (day-3b-solution 1)))
  (is (= 4 (day-3b-solution 2)))
  (is (= 5 (day-3b-solution 4)))
  (is (= 10 (day-3b-solution 5)))
  (is (= 11 (day-3b-solution 10)))
  (is (= 806 (day-3b-solution 750))))

(deftest valid-day-4a-passphrase?-test
  (is (true? (valid-day-4a-passphrase? "aa bb cc dd ee")))
  (is (false? (valid-day-4a-passphrase? "aa bb cc dd aa")))
  (is (true? (valid-day-4a-passphrase? "aa bb cc dd aaa"))))

(deftest valid-day-4b-passphrase?-test
  (is (true? (valid-day-4b-passphrase? "abcde fghij")))
  (is (false? (valid-day-4b-passphrase? "abcde xyz ecdab")))
  (is (true? (valid-day-4b-passphrase? "a ab abc abd abf abj")))
  (is (true? (valid-day-4b-passphrase? "iiii oiii ooii oooi oooo")))
  (is (false? (valid-day-4b-passphrase? "oiii ioii iioi iiio"))))

(deftest day-5a-solution-test
  (is (= 5 (day-5a-solution "0\n3\n0\n1\n-3\n"))))

(deftest day-5b-solution-test
  (is (= 10 (day-5b-solution "0\n3\n0\n1\n-3\n"))))

(deftest day-6a-solution-impl-test
  (is (= 5 (day-6a-solution "0 2 7 0"))))

(deftest day-6b-solution-impl-test
  (is (= 4 (day-6b-solution "0 2 7 0"))))

(deftest day-7a-solution-test
  (is (= "tknk" (day-7a-solution "pbga (66)
                                  xhth (57)
                                  ebii (61)
                                  havc (66)
                                  ktlj (57)
                                  fwft (72) -> ktlj, cntj, xhth
                                  qoyq (66)
                                  padx (45) -> pbga, havc, qoyq
                                  tknk (41) -> ugml, padx, fwft
                                  jptl (61)
                                  ugml (68) -> gyxo, ebii, jptl
                                  gyxo (61)
                                  cntj (57)"))))

(deftest day-7b-solution-test
  (is (= 60 (day-7b-solution "pbga (66)
                              xhth (57)
                              ebii (61)
                              havc (66)
                              ktlj (57)
                              fwft (72) -> ktlj, cntj, xhth
                              qoyq (66)
                              padx (45) -> pbga, havc, qoyq
                              tknk (41) -> ugml, padx, fwft
                              jptl (61)
                              ugml (68) -> gyxo, ebii, jptl
                              gyxo (61)
                              cntj (57)"))))

(deftest day-8a-solution-test
  (is (= 1 (day-8a-solution "b inc 5 if a > 1
                             a inc 1 if b < 5
                             c dec -10 if a >= 1
                             c inc -20 if c == 10"))))

(deftest day-8b-solution-test
  (is (= 10 (day-8b-solution "b inc 5 if a > 1
                              a inc 1 if b < 5
                              c dec -10 if a >= 1
                              c inc -20 if c == 10"))))

(deftest day-9a-solution-test
  (is (= 1 (day-9a-solution "{}")))
  (is (= 6 (day-9a-solution "{{{}}}")))
  (is (= 5 (day-9a-solution "{{},{}}")))
  (is (= 16 (day-9a-solution "{{{},{},{{}}}}")))
  (is (= 1 (day-9a-solution "{<a>,<a>,<a>,<a>}")))
  (is (= 9 (day-9a-solution "{{<ab>},{<ab>},{<ab>},{<ab>}}")))
  (is (= 9 (day-9a-solution "{{<!!>},{<!!>},{<!!>},{<!!>}}")))
  (is (= 3 (day-9a-solution "{{<a!>},{<a!>},{<a!>},{<ab>}}"))))

(deftest day-9b-solution-test
  (is (= 0 (day-9b-solution "<>")))
  (is (= 17 (day-9b-solution "<random characters>")))
  (is (= 3 (day-9b-solution "<<<<>")))
  (is (= 2 (day-9b-solution "<{!>}>")))
  (is (= 0 (day-9b-solution "<!!>")))
  (is (= 0 (day-9b-solution "<!!!>>")))
  (is (= 10 (day-9b-solution "<{o\"i!a,<{i<a>"))))

(deftest day-10a-solution-impl-test
  (is (= '(3 4 2 1 0) (calculate-hash 5 [3 4 1 5]))))

(deftest day-10b-solution-test
  (is (= "a2582a3a0e66e6e86e3812dcb672a272" (day-10b-solution "")))
  (is (= "33efeb34ea91902bb2f59c9920caa6cd" (day-10b-solution "AoC 2017")))
  (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (day-10b-solution "1,2,3")))
  (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (day-10b-solution "1,2,4"))))

(deftest day-11a-solution-test
  (is (= 3 (day-11a-solution "ne,ne,ne")))
  (is (= 0 (day-11a-solution "ne,ne,sw,sw")))
  (is (= 2 (day-11a-solution "ne,ne,s,s")))
  (is (= 3 (day-11a-solution "se,sw,se,sw,sw"))))

(deftest day-11b-solution-test
  (is (= 3 (day-11b-solution "ne,ne,ne")))
  (is (= 2 (day-11b-solution "ne,ne,sw,sw")))
  (is (= 2 (day-11b-solution "ne,ne,s,s")))
  (is (= 3 (day-11b-solution "se,sw,se,sw,sw"))))

(deftest day-12a-solution-test
  (is (= 6 (day-12a-solution "0 <-> 2
                              1 <-> 1
                              2 <-> 0, 3, 4
                              3 <-> 2, 4
                              4 <-> 2, 3, 6
                              5 <-> 6
                              6 <-> 4, 5"))))

(deftest day-12b-solution-test
  (is (= 2 (day-12b-solution "0 <-> 2
                              1 <-> 1
                              2 <-> 0, 3, 4
                              3 <-> 2, 4
                              4 <-> 2, 3, 6
                              5 <-> 6
                              6 <-> 4, 5"))))

(deftest day-13a-solution-test
  (is (= 24 (day-13a-solution "0: 3
                               1: 2
                               4: 4
                               6: 4"))))

(deftest day-13b-solution-test
  (is (= 10 (day-13b-solution "0: 3
                               1: 2
                               4: 4
                               6: 4"))))

(deftest ^:slow day-14a-solution-test
  (is (= 8108 (day-14a-solution "flqrgnkx"))))

(deftest ^:slow day-14b-solution-test
  (is (= 1231 (day-14b-solution "flqrgnkx"))))

(deftest ^:slow day-15a-solution-test
  (is (= 588 (day-15a-solution 65 8921))))

(deftest ^:slow day-15b-solution-test
  (is (= 309 (day-15b-solution 65 8921))))

(deftest day-16-solution-impl-test
  (is (= "baedc" (day-16-solution-impl "abcde" 1 "s1,x3/4,pe/b")))
  (is (= "ceadb" (day-16-solution-impl "abcde" 2 "s1,x3/4,pe/b"))))

(deftest day-17a-solution-test
  (is (= 638 (day-17a-solution 3))))

(deftest ^:slow day-17b-solution-test
  (is (= 1080289 (day-17b-solution 363))))

(deftest day-18a-solution-test
  (is (= 4 (day-18a-solution "set a 1
                              add a 2
                              mul a a
                              mod a 5
                              snd a
                              set a 0
                              rcv a
                              jgz a -1
                              set a 1
                              jgz a -2"))))

(deftest day-18-prog-send-message-test
  (let [prog (init-prog 1)]
    (is (-> prog (contains? :to-send) false?))
    (is (= 101 (->> prog (send-value 101) :to-send)))
    (is (= 1 (->> prog (send-register \p) :to-send)))
    (is (= 0 (->> prog (send-register \a) :to-send)))))

(deftest day-18-prog-receive-message-test
  (let [prog (assoc (init-prog 1) :current-instruction 0 :next-instruction 1)]
    (is (-> prog (contains? :message-queue)))
    (is (-> prog :message-queue empty?))
    (is (= {\p 1} (:registers prog)))
    (let [prog (receive-message \p prog)]
      (is (= {\p 1} (:registers prog)))
      (is (= 0 (:current-instruction prog)))
      (is (= 0 (:next-instruction prog))))
    (let [prog (enqueue-message prog 9)]
      (is (-> prog :message-queue seq))
      (is (= {\p 1} (:registers prog)))
      (is (= {\p 1 \a 9} (->> prog (receive-message \a) :registers)))
      (let [prog (receive-message \p prog)]
        (is (= {\p 9} (:registers prog)))
        (is (= 0 (:current-instruction prog)))
        (is (= 1 (:next-instruction prog)))
        (is (-> prog :message-queue empty?))))))

(deftest day-18-prog-jump-test
  (let [prog (assoc (init-prog 1) :current-instruction 5 :next-instruction 6)]
    (is (= {\p 1} (:registers prog)))
    (is (= 15 (->> prog (jump-if-pos \p 10) :next-instruction)))
    (is (= 6 (->> prog (jump-if-pos \a 10) :next-instruction)))))

(deftest day-18b-solution-test
  (is (= 0 (day-18b-solution "rcv a")))
  (is (= 1 (day-18b-solution "snd 9
                              rcv a")))
  (is (= 2 (day-18b-solution "snd 9
                              rcv b
                              snd 10
                              rcv a")))
  (is (= 0 (day-18b-solution "jgz p 2
                              rcv a")))
  (is (= 1 (day-18b-solution "jgz p 3
                              rcv a
                              jgz a 10
                              snd 9")))
  (is (= 1 (day-18b-solution "set a 10
                              set b 7
                              mod a b
                              add a -3
                              jgz a -1000
                              snd a")))
  (is (= 0 (day-18b-solution "set a 10
                              set b 7
                              mod a b
                              add a -2
                              jgz a -1000
                              snd a")))
  (is (= 3 (day-18b-solution "snd 1
                              snd 2
                              snd p
                              rcv a
                              rcv b
                              rcv c
                              rcv d"))))

(deftest day-19-solutions-test
  (let [puzzle (clojure.string/join "\n" ["     |          "
                                          "     |  +--+    "
                                          "     A  |  C    "
                                          " F---|----E|--+ "
                                          "     |  |  |  D "
                                          "     +B-+  +--+ "])] ; hack to prevent code automatic code formatting from messing with whitespace in puzzle
    (is (= "ABCDEF" (day-19a-solution puzzle)))
    (is (= 38 (day-19b-solution puzzle)))))

(deftest day-20-solutions-test
  (is (= 0 (day-20a-solution "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
                              p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>")))
  (is (= 1 (day-20b-solution "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
                              p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
                              p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
                              p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"))))

(deftest day-21-solutions-test
  (is (= 12 (day-21a-solution-impl 2 "../.# => ##./#../...
                                      .#./..#/### => #..#/..../..../#..#"))))

(let [grid "..#
            #..
            ..."]
  (deftest day-22-solutions-test
    (is (= 41 (day-22a-solution-impl 70 grid)))
    (is (= 5587 (day-22a-solution-impl 10000 grid)))
    (is (= 26 (day-22b-solution-impl 100 grid))))
  (deftest ^:slow day-22-slow-tests
    (is (= 2511944 (day-22b-solution-impl 10000000 grid)))))

