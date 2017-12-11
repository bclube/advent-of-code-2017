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
