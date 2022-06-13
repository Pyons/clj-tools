(ns pyons.tools)

(defmacro define
  "Inspired from scheme to shortcut functions which return a
  lambda function"
  [name binding lambda-binding & body]
  (let [doc-str? (if (string? (first body))
                   (first body)
                   "define scheme inspired")
        body (if (string? (first body))
               (next body)
               body)]
    `(defn ~name ~doc-str?
       [~@binding]
       (fn [~@lambda-binding]
         ~@body))))


(comment
  
  (macroexpand-1
    '(define test [a] [b] (+ a b)))

  (define test [a] [b] (+ a b))
  
  
  )
