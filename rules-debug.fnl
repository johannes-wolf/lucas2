(local trace (require :trace))
(local rule (require :rule))
(local list (require :list))
(import-macros em :macros)

(local environ (require :env))
(local env environ.global)

(lambda trace-set [{: mode}]
  (let [on (case mode
             [:int 0] false
             [:int _] true
             [:str :on] true
             [:str :off] false
             _ (error "Expected 0/1 or :on/:off."))]
    (tset trace :enable on)
    [:int (if on 1 0)]))

(lambda trace-get []
  [:int (if trace.enable 1 0)])

(lambda print-expr [e]
  (case e
    [:int n] (tostring n)
    [:fral n d] (string.format "%d:%d" n d)
    [:real f] (string.format "%.6f" f)
    [:str s] s
    [:list & items] (.. "[" (table.concat (list.map items print-expr) ", ") "]")
    _ ""))

(lambda print-msg [{: args}]
  (let [msg (fcollect [i 2 (length args)]
              (print-expr (. args i)))]
    (print (table.concat msg " "))
    [:ident :ok]))

(lambda order-list [{: args}]
  (local order (require :order))
  (let [l (case args
            [:list & args] args)]
    (table.sort l order.front)
    [:list (table.unpack l)]))

(local dbg-rules {:name "debug"
                  :rules [(rule.new "trace(mode)" trace-set)
                          (rule.new "quote(trace)" trace-get)
                          (rule.new "apply(quote(print),args)" print-msg)
                          (rule.new "canonical(args)" order-list)]})

(env:put-rules :debug dbg-rules)
