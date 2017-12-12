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
