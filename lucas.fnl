(local expr (require :expr))
(local rule (require :rule))
(local calc (require :calc))
(local list (require :list))
(local trace (require :trace))
(local simplify (require :simplify))
;(tset trace :enable true)

(local environ (require :env))
(local env environ.global)

(require :rules-ruleset)
(require :rules-debug)

;; Constants
(local sym-ok [:ident "ok"])

(env:set-attribute "hold" :hold :all)
(env:set-attribute "match" :hold :first)
(env:set-attribute "::" :hold :all)
(env:set-attribute "*" :orderless true)
(env:set-attribute "+" :orderless true)
(env:set-attribute "*" :flatten true)
(env:set-attribute "+" :flatten true)

; Default runtime ruleset
(local runtime-ruleset {:name "runtime" :rules []})
;(env:put-rules runtime-ruleset)

(fn fn-remove-rules [pattern env]
  "Remove all rules that match PATTERN."
  (var affected 0)
  (tset runtime-ruleset :rules
        (icollect [_ r (ipairs runtime-ruleset.rules)]
          (if (not (rule.match-expr pattern r.pattern env {}))
              r
              (set affected (+ affected 1)))))
  [:int affected])

(fn fn-store-const [ident expr env]
  "Assign exrpession to variable."
  (case ident
    [:ident _] (do (fn-remove-rules [:quote [:ident ident]] env)
                   (env:put-rules :pre-simplify (rule.new [:quote ident] expr))
                   sym-ok)
    _ [:str (string.format "Store target must be an identifier.")]))

(fn fn-store-rule [pattern expr env]
  "Assign pattern to replacement."
  (do (env:put-rules :simplify [(rule.new pattern expr)])
      sym-ok))

(fn eval [expr env]
  "Evaluate top level EXPR."
  (case expr
    [":==" ident value] (fn-store-rule [:quote ident] [:quote value] env)
    [":=" pattern value] (fn-store-rule pattern value env)
    ["env"] (do (env:print) [:int 1])
    expr (simplify.full expr env)))

(var n 0)
(var last-ok true)
(var ans nil)

(local repl-ruleset {:name "repl"
                     :rules [(rule.new "quote(ans)" #(if ans ans [:int 0]))
                             ]})
(env:put-rules :repl repl-ruleset)

;; Load and evaluate file line per line
(fn dofile [file]
  (let [time (os.time)]
    (pcall #(do (each [in (io.lines file)]
                  (when (and (not (string.match in "^%s*;;"))
                             (not (string.match in "^%s*$")))
                    (let [(ok err) (xpcall (lambda []
                                             (eval (expr.compile in) env)) debug.traceback)]
                      (when (not ok)
                          (print (string.format "Error (init): %s" err))
                          (print (debug.traceback)))))) 
                (print (string.format "[OK ]> %s in %.2fs" file (os.difftime (os.time) time)))))))

(dofile ".lucasrc")
(dofile (.. (os.getenv "HOME") "/.lucasrc"))

(while true
  (io.write (string.format "[%s]> " (if last-ok "OK " "ERR")))
  (let [in (io.stdin:read :*line)]
    (when (not in)
      (os.exit 0))
    (let [(ok err) (xpcall (lambda []
                             (let [e (expr.compile in)]
                               (let [result (eval e env)]
                                 (set ans result)
                                 (print (string.format "%4s = %s"
                                                       (string.format "%%%d" n)
                                                       (expr.to-string result)))
                                 ))) debug.traceback)]
      (set last-ok (not (not ok)))
      (if (not ok)
          (do (print (string.format "Error: %s" err))
              (print (debug.traceback))))
    (set n (+ n 1)))))
