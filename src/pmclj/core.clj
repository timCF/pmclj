(ns pmclj.core
    (:use [clojure.core.match :only (match)]))

(defmacro check_match [obj pattern]
      `(match ~obj
             ~pattern true
             :else false))

(defn pipe_matching_inner [{pattern :pattern, result :result, expressions :expressions}]
      (case (or (= expressions nil) (= expressions ()))
            true result
            false  (let [to_pipe (first expressions) rest_expr (rest expressions)]
                        `(let [~'res ~result]
                              (case (check_match ~'res ~pattern)
                                    true ~(pipe_matching_inner {:pattern pattern, :result `(-> ~'res ~to_pipe), :expressions rest_expr})
                                    false ~'res)))))

(defmacro pipe_matching [pattern init_expression & other_expressions ]
          (pipe_matching_inner {:pattern pattern, :result init_expression, :expressions other_expressions}))

