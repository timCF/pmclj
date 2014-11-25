(ns pmclj.core
    (:use [clojure.core.match :only (match)]))

;; macro for debug

(defmacro show_it [expr]
  `(let [res# ~expr]
     (println res#)
     res#))

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
;;; recursive key matching ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro map_iterate_macro [key]
  `(fn [res# el#]
     (match res#
            nil (match (first el#)
                       ~key (zipmap [(first el#)] [(last el#)])
                       _# (recurs_get (last el#) ~key))
            _# res#)))

(defn recurs_get [some_map key]
  (case (map? some_map)
    false nil
    true (reduce (map_iterate_macro key) nil (seq some_map))))

(defmacro rkey_matching [key init_expr & others_expr ]
  (case (or (= others_expr nil) (= others_expr ()))
    true   init_expr
    false  `(let [res# ~init_expr]
              (case (recurs_get res# ~key)
                nil  res#
                (rkey_matching ~key (-> res# ~(first others_expr)) ~@(rest others_expr))))))

(defmacro rkey_not_matching [key init_expr & others_expr ]
  (case (or (= others_expr nil) (= others_expr ()))
    true   `(let [res# ~init_expr]
              (match (recurs_get res# ~key)
                     nil res#
                     some_map# some_map#))
    false  `(let [res# ~init_expr]
              (match (recurs_get res# ~key)
                     nil  (rkey_not_matching ~key (-> res# ~(first others_expr)) ~@(rest others_expr))
                     some_map# some_map#))))




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