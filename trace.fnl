(local trace {})
(local list (require :list))

(macro if-trace [body]
  `(if trace.enable
       (do ,body)))

(fn to-string [v]
  (local expr (require :expr))
  (if (not v)
      "nil"
      (= (type v) :table)
      (if (= (length v) 0)
          "()"
          (= (type (. v 1)) :string)
          (expr.to-string v)
          (.. "(" (table.concat (list.map (list.copy v [])
                                          to-string) " ") ")"))
      (tostring v)))

(fn trace.result [value]
  (if-trace (print (.. " -> " (to-string value))))
  value)

(fn trace.step [msg ...]
  (if-trace (let [val (icollect [_ v (ipairs [...])]
                        (to-string v))
                  msg (.. msg ": " (table.concat val ", "))]
              (print msg))))

trace
