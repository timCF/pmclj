(ns pmclj.core-test
    (:require [clojure.test :refer :all]
     [pmclj.core :refer :all]))



(defn func1 [arg2]
      (println "func1 called" arg2)
      {:ok "func1_returns"})

(defn func2 [arg1 arg2]
      (println "func2 called" arg1 arg2)
      {:fail "func2_returns"})

(defn func3 [arg1 arg2]
      (println "func3 called" arg1 arg2)
      {:ok "func3_returns"})
(defn proxy_func [arg1 arg2] arg2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pred matching tests ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn example1_pred []
      (pred_matching #(contains? % :ok) (func1 123) (func2 123) (func3 123)))
(defn example2_pred []
      (pred_matching #(contains? % :ok) (func1 123) (func3 123) (func2 123)))
(defn example3_pred []
      (pred_not_matching #(contains? % :fail) (func1 123) (func2 123) (func3 123)))
(defn example4_pred []
      (pred_not_matching #(contains? % :fail) (func1 123) (func3 123) (func2 123)))
(defn example5_pred [] (let [res# "qwe"] (pred_matching #(contains? % :ok) (func1 res#) (func3 res#) (proxy_func res#))))
(defn example6_pred [] (let [res "qwe"] (pred_matching #(contains? % :ok) (func1 res) (func3 res) (proxy_func res))))




(deftest a-test1_pred
         (testing "example1"
                  (is (= {:fail "func2_returns"} (example1_pred)))))
(deftest a-test2_pred
         (testing "example2"
                  (is (= {:fail "func2_returns"} (example2_pred)))))
(deftest a-test3_pred
         (testing "example3"
                  (is (= {:fail "func2_returns"} (example3_pred)))))
(deftest a-test4_pred
         (testing "example4"
                  (is (= {:fail "func2_returns"} (example4_pred)))))
(deftest a-test5_pred
         (testing "example5"
                  (is (= "qwe" (example5_pred)))))
(deftest a-test6_pred
         (testing "example6"
                  (is (= "qwe" (example6_pred)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pipe matching tests ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn example1_pm []
      (pipe_matching {:ok some} (func1 123) (func2 123) (func3 123)))
(defn example2_pm []
      (pipe_matching {:ok some} (func1 123) (func3 123) (func2 123)))
(defn example3_pm []
      (pipe_not_matching {:fail some} (func1 123) (func2 123) (func3 123)))
(defn example4_pm []
      (pipe_not_matching {:fail some} (func1 123) (func3 123) (func2 123)))
(defn example5_pm [] (let [res# "qwe"] (pipe_matching {:ok some} (func1 res#) (func3 res#) (proxy_func res#))))
(defn example6_pm [] (let [res "qwe"] (pipe_matching {:ok some} (func1 res) (func3 res) (proxy_func res))))




(deftest a-test1_pm
         (testing "example1"
                  (is (= {:fail "func2_returns"} (example1_pm)))))
(deftest a-test2_pm
         (testing "example2"
                  (is (= {:fail "func2_returns"} (example2_pm)))))
(deftest a-test3_pm
         (testing "example3"
                  (is (= {:fail "func2_returns"} (example3_pm)))))
(deftest a-test4_pm
         (testing "example4"
                  (is (= {:fail "func2_returns"} (example4_pm)))))
(deftest a-test5_pm
         (testing "example5"
                  (is (= "qwe" (example5_pm)))))
(deftest a-test6_pm
         (testing "example6"
                  (is (= "qwe" (example6_pm)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; key matching tests ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn example1_km []
      (key_matching :ok (func1 123) (func2 123) (func3 123)))
(defn example2_km []
      (key_matching :ok (func1 123) (func3 123) (func2 123)))
(defn example3_km []
      (key_not_matching :fail (func1 123) (func2 123) (func3 123)))
(defn example4_km []
      (key_not_matching :fail (func1 123) (func3 123) (func2 123)))
(defn example5_km [] (let [res# "qwe"] (key_matching :ok (func1 res#) (func3 res#) (proxy_func res#))))
(defn example6_km [] (let [res "qwe"] (key_matching :ok (func1 res) (func3 res) (proxy_func res))))




(deftest a-test1_km
         (testing "example1"
                  (is (= {:fail "func2_returns"} (example1_km)))))
(deftest a-test2_km
         (testing "example2"
                  (is (= {:fail "func2_returns"} (example2_km)))))
(deftest a-test3_km
         (testing "example3"
                  (is (= {:fail "func2_returns"} (example3_km)))))
(deftest a-test4_km
         (testing "example4"
                  (is (= {:fail "func2_returns"} (example4_km)))))
(deftest a-test5_km
         (testing "example5"
                  (is (= "qwe" (example5_km)))))
(deftest a-test6_km
         (testing "example6"
                  (is (= "qwe" (example6_km)))))