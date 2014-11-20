(ns pmclj.core
    (:use [clojure.core.match :only (match)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; predicate matching ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro pred_matching [pred init_expr & others_expr ]
          (case (or (= others_expr nil) (= others_expr ()))
                true   init_expr
                false  `(let [res# ~init_expr]
                             (case (~pred res#)
                                   true  (pred_matching ~pred (-> res# ~(first others_expr)) ~@(rest others_expr))
                                   false res#))))

(defmacro pred_not_matching [pred init_expr & others_expr ]
          (case (or (= others_expr nil) (= others_expr ()))
                true   init_expr
                false  `(let [res# ~init_expr]
                             (case (~pred res#)
                                   false  (pred_not_matching ~pred (-> res# ~(first others_expr)) ~@(rest others_expr))
                                   true res#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; full pattern matching ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro check_match [obj pattern]
      `(match ~obj
             ~pattern true
             :else false))

(defmacro pipe_matching [pattern init_expr & others_expr ]
          (case (or (= others_expr nil) (= others_expr ()))
                true   init_expr
                false  `(let [res# ~init_expr]
                             (case (check_match res# ~pattern)
                                   true  (pipe_matching ~pattern (-> res# ~(first others_expr)) ~@(rest others_expr))
                                   false res#))))

(defmacro pipe_not_matching [pattern init_expr & others_expr ]
          (case (or (= others_expr nil) (= others_expr ()))
                true   init_expr
                false  `(let [res# ~init_expr]
                             (case (check_match res# ~pattern)
                                   false (pipe_not_matching ~pattern (-> res# ~(first others_expr)) ~@(rest others_expr))
                                   true  res#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; simplified key matching ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro key_matching [key init_expr & others_expr ]
          (case (or (= others_expr nil) (= others_expr ()))
                true   init_expr
                false  `(let [res# ~init_expr]
                             (case (get res# ~key)
                                   nil  res#
                                   (key_matching ~key (-> res# ~(first others_expr)) ~@(rest others_expr))))))

(defmacro key_not_matching [key init_expr & others_expr ]
          (case (or (= others_expr nil) (= others_expr ()))
                true   init_expr
                false  `(let [res# ~init_expr]
                             (case (get res# ~key)
                                   nil  (key_not_matching ~key (-> res# ~(first others_expr)) ~@(rest others_expr))
                                   res#))))