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
  (is (= '(3 4 2 1 0) (day-10a-solution-impl 5 [3 4 1 5]))))

(deftest day-10b-solution-test
  (is (= "a2582a3a0e66e6e86e3812dcb672a272" (day-10b-solution "")))
  (is (= "33efeb34ea91902bb2f59c9920caa6cd" (day-10b-solution "AoC 2017")))
  (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (day-10b-solution "1,2,3")))
  (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (day-10b-solution "1,2,4"))))
