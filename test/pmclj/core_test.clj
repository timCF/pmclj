(ns pmclj.core-test
    (:require [clojure.test :refer :all]
     [pmclj.core :refer :all]))

(defn func1 [some_int]
      (println "func1 called" some_int)
      {:ok "func1_returns"})

(defn func2 [some_arg some_int]
      (println "func2 called" some_arg some_int)
      {:fail "func2_returns"})

(defn func3 [some_arg some_int]
      (println "func3 called" some_arg some_int)
      {:ok "func3_returns"})

(defn example1 []
      (pipe_matching {:ok some} (func1 123) (func2 123) (func3 123)))
(defn example2 []
      (pipe_matching {:ok some} (func1 123) (func3 123) (func2 123)))
(defn example3 []
      (pipe_not_matching {:fail some} (func1 123) (func2 123) (func3 123)))
(defn example4 []
      (pipe_not_matching {:fail some} (func1 123) (func3 123) (func2 123)))


(deftest a-test1
         (testing "example1"
                  (is (= {:fail "func2_returns"} (example1)))))
(deftest a-test2
         (testing "example2"
                  (is (= {:fail "func2_returns"} (example2)))))
(deftest a-test3
         (testing "example3"
                  (is (= {:fail "func2_returns"} (example3)))))
(deftest a-test4
         (testing "example4"
                  (is (= {:fail "func2_returns"} (example4)))))