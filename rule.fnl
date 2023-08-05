(local expression (require :expr))
(local environ (require :env))
(local simplify (require :simplify))
(local list (require :list))
(local trace (require :trace))
(import-macros em :macros)

(fn make-set [l]
  "Make table from list."
  (let [t {}]
    (each [_ v (ipairs l)]
      (tset t v true))
    t))

(local rule {})
(tset rule :__index rule)

(tset rule :default-iterations 10000)
(tset rule :recursion-limit 10000)

;; List of variable names that are matched literally (like with quote(...))
(local constant-vars (make-set ["e" "pi" "i" "phi" "gamma"
                                "inf" "uinf" "nan" "nil"]))

(fn rule.apply [expr env rules ?n]
  "Apply rules from RULES from start to end. Repeat each single rule until it
   failes to match."
  (var expr expr)
  (let [n (or ?n rule.default-iterations)]
    (var i 0)
    (var changed true)
    (while (and (< i n) changed)
      (set i (+ 1 i))
      (set changed false)
      (each [_ r (ipairs rules)]
        (let [(res chng?) (r:reduce expr env)]
          (when chng?
            (set changed true)
            (set expr (simplify.simplify res env)))))))
  expr)

(fn rule.new [pattern replacement]
  "Instantiate a new rule object. The REPLACEMENT can be a function."
  (setmetatable {:pattern pattern
                 :replacement replacement
                 :compiled (not= (type pattern) "string")} rule))

(fn rule.compile [self]
  "Compile SELF. This is done automatically on first use."
  (when (not self.compiled)
    (when (= (type self.pattern) "string")
      (tset self :pattern (expression.compile self.pattern)))
    (when (= (type self.replacement) "string")
      (tset self :replacement (expression.compile self.replacement)))
    (tset self :compiled true)))

(fn match-rest-rec [pattern expr index env vars ?quote]
  "Match tail of EXPR against tail of PATTERN."
  (let [pattern* (. pattern index)
        expr* (. expr index)]
    (case pattern*
      _ (or (and (> index (length pattern))
                 (> index (length expr)))
            (and (<= index (length pattern))
                 (<= index (length expr))
                 (rule.match-expr pattern* expr* env vars ?quote)
                 (match-rest-rec pattern expr (+ index 1) env vars ?quote))))))

(fn match-rest-flat [kind pattern expr env vars ?quote]
  (trace.step "match-rest-flat" pattern expr)
  (let [pattern-len (length pattern)
        expr-len (length expr)]
    (if (= pattern-len expr-len)
        (match-rest-rec pattern expr 2 env ?quote)
        (< pattern-len expr-len)
        (do (var ok? true)
            (for [i 2 (- pattern-len 1)] :until (not ok?)
                 (set ok? (rule.match-expr (. pattern i)
                                           (. expr i)
                                           env
                                           vars
                                           ?quote)))
            (and ok? (rule.match-expr (. pattern pattern-len)
                                      (list.prepend kind
                                                    (list.slice expr pattern-len))
                                      vars
                                      env
                                      ?quote)))
        false)))

(fn match-rest [pattern expr env vars ?quote]
  (trace.step "match-rest" pattern expr)
  (let [flatten (env:get-attribute (em.kind pattern) :flatten)]
    (if flatten
        (match-rest-flat (em.kind pattern) pattern expr env vars ?quote)
        (match-rest-rec pattern expr 2 env vars ?quote))))

;; Matches any function call
;;
;; Never matches constants or lists, but can match operators.
;; Strings can be used as first argument.
;;
;; Examples:
;;   f(1,2) -> apply(quote(f),a) -> a=[1,2]
;;   q(1,2) -> apply(f,a)        -> q=f a=[1,2]
(fn match-apply [name args expr env vars ?quote]
  "Match function call against function name and argument list."
  (let [name (case name [:str n] [:ident n] else else)]
    (if (or (em.kind= expr :int)
            (em.kind= expr :frac)
            (em.kind= expr :real)
            (em.kind= expr :ident)
            (em.kind= expr :list))
        nil
        (case expr
          [f & fargs] (and (rule.match-expr name [:ident f]
                                            env vars ?quote)
                           (rule.match-expr args [:list (table.unpack fargs)]
                                            env vars ?quote))))))

;; Matches a lists head and tail
;;
;; Examples:
;;   [1,2,3] ->  cons(h,t) h=1 t=[2,3]
;;   [1,2,3] -> rcons(h,t) h=[1,2] t=3
(fn match-cons [head tail reverse expr env vars ?quote]
  "Match LIST against a head and tail pattern."
  (fn match-cons-int [head tail he te env ?quote]
    (trace.step "match-cons" head tail he te)
    (and (rule.match-expr head he env vars ?quote)
         (rule.match-expr tail te env vars ?quote)))
  (if (em.kind= expr :list)
      (if (not reverse)
          (let [he (. expr 2)
                te (list.prepend :list (list.slice expr 3))]
            (and he te (match-cons-int head tail he te env ?quote)))
          (let [he (. expr (length expr))
                te (list.prepend :list (list.slice expr 2 (- (length expr) 1)))]
            (and he te (match-cons-int head tail he te env ?quote))))))

(fn instantiate-expr [expr env vars]
  "Set all variables of EXPR to values of ENV.
   Panic if a value is missing."
  (trace.step "instantiate-expr" expr)

  (macro apply [e]
    `(instantiate-expr ,e env vars))
  (macro reduce [e]
    `(simplify.simplify (apply ,e) env))

  (case expr
    ;; Expand variables to their respective values
    ["ident" ident] (or (. vars ident)
                        (error (string.format "Unbound variable %s."
                                              ident)))
    ;; Do not expand variables insides quote body
    ["quote" body] body
    ;; Join lists
    ["cons" head tail] [:cons (reduce head) (reduce tail)]
    ["rcons" head tail] [:rcons (reduce head) (reduce tail)]
    ;; Apply call
    ["apply" name args] [:apply (reduce name) (reduce args)]
    ;; Recurse into children
    [head & rest] (setmetatable
                   [head (table.unpack (icollect [_ v (ipairs rest)]
                                         (apply v)))]
                   (getmetatable expr))
    ;; Forward plain values
    _ expr))

(lambda rule.match-expr [pattern expr env vars ?quote]
  "Match EXPR against PATTERN and fill ENV."
  ;(trace.step "match-expr" pattern expr)
  (case pattern
    ;; Capture variable if unset, else match expression against captured value
    ["ident" ident] (if (or ?quote (. constant-vars ident))
                        (match-rest pattern expr env vars ?quote)
                        (. vars ident)
                        (rule.match-expr (. vars ident) expr env vars true)
                        (do (trace.step "put-var" ident expr)
                            (tset vars ident expr) true))
    ;; Quote function disables variable capturing for its body
    ["quote" body] (match-rest body expr env vars true)
    ;; Rule condition must evaluate to true for this rule to match
    ["::" what cond] (let [ok? (rule.match-expr what expr env vars ?quote)]
                       (when ok?
                         (expression.true?
                          (rule.ruleset-reduce (instantiate-expr cond env)
                                               env
                                               vars))))
    ;; Match list head + tail... or head... + tail
    ["cons" head tail] (match-cons head tail false expr env vars ?quote)
    ["rcons" tail head] (match-cons head tail true expr env vars ?quote)
    ;; Match any call
    ["apply" name args] (match-apply name args expr env vars ?quote)
    ;; Match all list items
    [& _] (and (em.kind= pattern (em.kind expr))
               (match-rest pattern expr env vars ?quote))
    ;; Compare values for equality
    value (= value expr)))

(fn rule.match [self expr env vars]
  "Match expression EXPR against SELF, filling env."
  (self:compile)
  (rule.match-expr self.pattern expr env vars))

(fn rule.reduce-single [self expr env ?depth]
  "Tries to reduce EXPR via SELF non-recursive."
  (when (> ?depth rule.recursion-limit)
    (error (string.format "Rule recursion limit hit.")))

  (let [sub-env (environ.new env)
        vars {}]
    (if (self:match expr sub-env vars)
        (if (= (type self.replacement) "function")
            ;; Functions may return nil in which case we do not
            ;; rewrite the expression.
            (let [r (self.replacement vars)]
              (values (or r expr) (not= r nil)))
            ;; Rewrite using an instantiation of the replacement
            (values (instantiate-expr self.replacement sub-env vars) true))
        ;; No match
        (values expr false))))

(fn rule.reduce [self expr env ?depth]
  "Tries to reduce EXPR via SELF."
  (if (= (type expr) :table)
      (let [hold (env:get-attribute (em.kind expr) :hold)]
        (var ?depth (+ (or ?depth 0) 1))
        (var changes 0)
        (when (not= hold :all)
          (local num-args (length expr))
          (var first-arg 2)
          (var last-arg num-args)
          (case hold
            :first (set first-arg 3)
            :last  (set last-arg (- num-args 1))
            :rest  (set last-arg 3))

          (let [r (fcollect [i 1 (length expr)]
                    (if (and (>= i first-arg) (<= i last-arg))
                        (let [(reduced changed?) (self:reduce (. expr i) env ?depth)]
                          (if changed?
                              (set changes (+ changes 1)))
                          reduced)
                        (. expr i)))]
            (let [(reduced changed?) (self:reduce-single r env ?depth)]
              (when changed?
                (trace.step "rewrite" expr reduced)
                (set changes (+ changes 1)))
              (values reduced (> changes 0))))))
      (values expr false)))

rule
