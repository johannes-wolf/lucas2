(local calc (require :calc))
(local list (require :list))
(local alg (require :alg))
(local order (require :order))
(local trace (require :trace))
(local environ (require :env))
(local expression (require :expr))
(local operators (require :operators))
(import-macros em :macros)

(local simplify {})

(fn atom? [e]
  (or (em.kind= e :int)
      (em.kind= e :frac)
      (em.kind= e :real)
      (em.kind= e :ident)
      (em.kind= e :str)))

(fn contains? [l what]
  "Search list L for WHAT."
  (var found? false) 
  (each [_ v (ipairs l)] :until (= found? true)
        (set found? (order.compare v what)))
  found?)

(fn merge-operands [p q simplify-fn ...]
  (trace.step "merge-operands" p q)
  (if (or (not q) (= (length q) 0)) p
      (or (not p) (= (length p) 0)) q
      (let [p1 (. p 1) q1 (. q 1) h (simplify-fn [p1 q1] ...)]
        (if (= (length h) 0)
            (merge-operands (list.tail p) (list.tail q)
                            simplify-fn ...)
            (= (length h) 1)
            (list.join h (merge-operands (list.tail p) (list.tail q)
                                             simplify-fn ...))
            (and (= (length h) 2)
                 (order.compare (. h 1) p1)
                 (order.compare (. h 2) q1))
            (list.prepend p1 (merge-operands (list.tail p) q
                                             simplify-fn ...))
            (and (= (length h) 2)
                 (order.compare (. h 1) q1)
                 (order.compare (. h 2) p1))
            (list.prepend q1 (merge-operands p (list.tail q)
                                             simplify-fn ...))))))

(fn simplify.rational-number [u]
  "Simplify rational number."
  (case u
    [:frac 0 _] [:int 0]
    [:frac a a] [:int 1]
    [:real 0.0] [:int 0]
    [:real 1.0] [:int 1]
    _ u))

(fn simplify.rne-rec [u]
  "Simplify rantional number expression recursive."
  (trace.step "rne-rec" u)
  (assert (<= (em.args# u) 2))
  (let [num-args (em.args# u)]
    (if (or (em.kind= u :int) (em.kind= u :frac) (em.kind= u :real))
        u
        (= num-args 1)
        (let [v (simplify.rne-rec (em.arg u 1))]
          (case v
            ["+" a] v
            ["-" a] (calc.mul (em.int -1) a)
            _ v))
        (= num-args 2)
        (let [k (em.kind u)]
          (if (or (= k "+") (= k "*") (= k "-") (= k "/"))
              (let [v (simplify.rne-rec (em.arg u 1))
                    w (simplify.rne-rec (em.arg u 2))]
                (if (or (calc.nan? v) (calc.nan? w))
                    [:ident "nan"]
                    (if (= k "+")
                        (calc.add v w)
                        (= k "*")
                        (calc.mul v w)
                        (= k "-")
                        (calc.sub v w)
                        (= k "/")
                        (calc.div v w))))
              (= k "^")
              (let [v (simplify.rne-rec (em.arg u 1))]
                (if (calc.nan? v)
                    [:ident "nan"]
                    (calc.pow v (em.arg u 2)))))))))

(fn simplify.rne [u]
  "Simplify rational number expression."
  (trace.step "rne" u)
  (let [v (simplify.rne-rec u)]
    (if (calc.nan? v)
        v
        (simplify.rational-number v))))

(fn simplify.product-rec [l]
  "Simplify product arguments recursive"
  (trace.step "product-rec" l)
  (let [a (. l 1) b (. l 2)]
    (if (and (= (length l) 2) (not (em.kind= a "*")) (not (em.kind= b "*")))
        (if (and (calc.const? a) (calc.const? b))
            (let [r (simplify.rne ["*" a b])]
              (if (em.int= r 0)
                  []
                  [r]))
            (em.int= a 1)
            [b]
            (em.int= b 1)
            [a]
            (order.compare (alg.base a) (alg.base b))
            (let [s (simplify.sum ["+" (alg.exponent a) (alg.exponent b)])
                  p (simplify.power ["^" (alg.base a) s])]
              (if (em.int= p 1) []
                  [p]))
            (order.front b a)
            [b a]
            l)
        (and (= (length l) 2) (or (em.kind= a "*") (em.kind= b "*")))
        (if (and (em.kind= a "*") (em.kind= b "*"))
            (merge-operands (list.tail a) (list.tail b) simplify.product-rec)
            (em.kind= a "*")
            (merge-operands (list.tail a) [b] simplify.product-rec)
            (em.kind= b "*")
            (merge-operands [a] (list.tail b) simplify.product-rec))
        (> (length l) 2)
        (let [w (simplify.product-rec (list.tail l))]
          (if (em.kind= a "*")
              (merge-operands (list.tail a) w simplify.product-rec)
              (merge-operands [a] w simplify.product-rec))))))

(fn simplify.product [u]
  "Simplify product expression."
  (trace.step "product" u)
  (if (contains? u [:ident "nan"])
      [:ident "nan"]
      (contains? u [:int 0])
      (em.int 0)
      (= (em.args# u) 1)
      (em.arg u 1)
      (let [v (simplify.product-rec (list.tail u))]
        (if (= (length v) 1)
            (. v 1)
            (and (= (length v) 2) (calc.const? (. v 1)) (em.kind= (. v 2) "+"))
            (list.map (. v 2) #(simplify.product ["*" (. v 1) $1]) 2)
            (and (= (length v) 2) (calc.const? (. v 2)) (em.kind= (. v 1) "+"))
            (list.map (. v 1) #(simplify.product ["*" (. v 2) $1]) 2)
            (>= (length v) 2)
            (list.prepend "*" v)
            (= (length v) 0)
            (em.int 1)
            ))))

(fn simplify.sum-rec [l]
  "Simplify list of sum arguments."
  (trace.step "sum-rec" l)
  (let [a (. l 1) b (. l 2)]
    (if (and (= (length l) 2) (not (em.kind= a "+")) (not (em.kind= b "+")))
        (if (and (calc.const? a) (calc.const? b))
            (let [r (simplify.rne ["+" a b])]
              (if (em.int= r 0) []
                  [r]))
            (em.int= a 0)
            [b]
            (em.int= b 0)
            [a]
            (order.compare (alg.term a) (alg.term b))
            (let [s (simplify.sum ["+" (alg.const a) (alg.const b)])
                  p (simplify.product ["*" s (alg.term a)])]
              (if (em.int= p 0) []
                  [p]))
            (order.front b a)
            [b a]
            l)
        (and (= (length l) 2) (or (em.kind= a "+") (em.kind= b "+")))
        (if (and (em.kind= a "+") (em.kind= b "+"))
            (merge-operands (list.tail a) (list.tail b) simplify.sum-rec)
            (em.kind= a "+")
            (merge-operands (list.tail a) [b] simplify.sum-rec)
            (em.kind= b "+")
            (merge-operands [a] (list.tail b) simplify.sum-rec))
        (> (length l) 2)
        (let [w (simplify.sum-rec (list.tail l))]
          (if (em.kind= a "+")
              (merge-operands (list.tail a) w simplify.sum-rec)
              (merge-operands [a] w simplify.sum-rec))))))

(fn simplify.sum [u]
  (trace.step "sum" u)
  (assert (em.kind= u "+"))
  (if (contains? u [:ident "nan"])
      [:ident "nan"]
      (= (em.args# u) 1)
      (em.arg u 1)
      (let [v (simplify.sum-rec (list.tail u))]
        (if (= (length v) 1)
            (. v 1)
            (>= (length v) 2)
            (list.prepend "+" v)
            (= (length v) 0)
            (em.int 0)))))

(fn simplify.power [u]
  (trace.step "power" u)
  (assert (em.kind= u "^"))
  (let [b (em.arg u 1) e (em.arg u 2)]
    (if (calc.const? b)
        (simplify.rne u)
        (and (em.kind= b "^") (em.kind= e :int))
        (let [r (em.arg b 1)
              s (em.arg b 2)
              p (simplify.product ["*" s e])]
          (simplify.power ["^" r p]))
        (em.kind= b "*")
        (let [r (list.map b #(simplify.power ["^" $1 e]) 2)]
          (simplify.product r))
        (em.int= e 0)
        (em.int 1)
        (em.int= e 1)
        b
        (and (em.kind= b :ident) (= (em.arg b 1) "e"))
        ["exp" e]
        u)))
  
(fn simplify.quotient [u]
  (assert (em.kind= u "/"))
  (let [p (simplify.power ["^" (em.arg u 2) (em.int -1)])]
    (simplify.product ["*" (em.arg u 1) p])))

(fn simplify.difference [u]
  (assert (em.kind= u "-"))
  (if (= (em.args# u) 1)
      (simplify.product ["*" (em.int -1) (em.arg u 1)])
      (let [a (em.arg u 1)
            b (em.arg u 2)
            d (simplify.product ["*" (em.int -1) b])]
        (simplify.sum ["+" a d]))))

(fn simplify.flatten [e]
  "Flatten expression E."
  (trace.step "flatten" e)
  (let [kind (em.kind e)]
    (var l [kind])
    (fn flatten-rec [p]
      (if (em.kind= p kind)
          (for [i 2 (length p)]
            (flatten-rec (. p i)))
          (table.insert l p)))
    (flatten-rec e)
    l))

(fn simplify.orderless [e]
  (trace.step "orderless" e)
  (let [args (list.slice e 2)]
    (table.sort args order.front)
    [(. e 1) (table.unpack args)]))

(fn get-simplify-range [e hold-mode]
  "Get range of arguments that must get simplified for the given hold mode."
  (let [len (length e)]
    (case hold-mode
      :all nil
      :first {:begin 3 :end len}
      :last {:begin len :end len}
      _ {:begin 2 :end len})))

(fn flatten-expr [e env]
  (let [flatten-expr (env:get-attribute (em.kind e) :flatten)]
    (if (and flatten-expr
             (not (em.kind= e "*"))
             (not (em.kind= e "+")))
        (simplify.flatten e)
        e)))

(fn order-expr [e env]
  (let [orderless (env:get-attribute (em.kind e) :orderless)]
    (if (and orderless
             (not (em.kind= e "*"))
             (not (em.kind= e "+")))
        (simplify.orderless e)
        e)))

(fn simplify-expr [e env]
  (let [hold-mode (env:get-attribute (em.kind e) :hold)
        range (get-simplify-range e hold-mode)]
    (if range
        (list.map e #(simplify.simplify $1 env) range.begin range.end)
        e)))

(lambda simplify.simplify-rec [e env]
  (-> e
      (flatten-expr env)
      (order-expr env)
      (simplify-expr env)))

(lambda simplify.cons [e rev]
  (let [head (if rev (em.arg e 2) (em.arg e 1))
        tail (case (if rev (em.arg e 1) (em.arg e 2)) [:list & args] args _ nil)]
    (if (and head tail)
        (if rev
            (list.prepend :list (list.append head tail))
            [:list head (table.unpack (or tail []))])
        e)))

;; Apply a list of arguments to a function
;;
;; Examples
;;   apply(:sin, [x])
;;   apply(:table, [x^2,[x,1,10]])
(lambda simplify.apply [e env]
  (let [ident (case (em.arg e 1)
                [:str s] s
                _ nil)
        args (case (em.arg e 2)
               [:list & l] l
               _ nil)]
    (if (and ident args)
        (simplify.simplify [ident (table.unpack args)] env)
        e)))

;; Function for generating sequences
;;
;; Examples
;;   table(x,3)         -> [x,x,x]
;;   table(x,[x,3])     -> [1,2,3]
;;   table(x,[x,3,6])   -> [3,4,5,6]
;;   table(x,[x,2,6,2]) -> [2,4,6]
(lambda simplify.table [e env]
  (local rule (require :rule))
  (fn table-impl [s v from to step]
    (if (not v)
        [:list (table.unpack (fcollect [i 1 to] s))]
        [:list (table.unpack (fcollect [i from to step]
                                       (let [r (rule.new [:quote v] #[:int i])]
                                         (-> s
                                             (r:reduce env 0)
                                             (simplify.simplify env)))))]))

  (let [s (em.arg e 1)
        r (em.arg e 2)]
    (case r
      [:int to] (table-impl s nil 1 to 1)
      [:list v [:int from] [:int to] [:int step]] (table-impl s v from to step)
      [:list v [:int from] [:int to]] (table-impl s v from to (if (<= from to) 1 -1))
      [:list v [:int to]] (table-impl s v 1 to 1)
      _ e)))

(macro if-signature [e arg-from arg-to body]
  `(if (and ,e (>= (- (length ,e) 1) ,arg-from)
               (or (= ,arg-to :inf) (<= (- (length ,e) 1) ,arg-to)))
       ,body
       ,e))

(fn simplify.length [e]
  (if-signature e 1 1
                (case (em.arg e 1)
                  [:int _] 0
                  [:real _] 0
                  [:frac _ _] 0
                  [:ident _] 0
                  [:str s] [:int (length s)]
                  [head & items] [:int (length items)]
                  _ e)))

;; Get element at position(s)
;; Auto releases its first argument.
;;
;; Examples
;;  at([4,5,6], 2) -> 5
;;  at([[1,2,3],[4,5,6]], 2, 1) -> 4
;;  at(hold(4+5),2) -> 5
;;  at("abc", 2) -> "b"
(fn simplify.nth [e]
  (fn at-impl [e pos n]
    (if (and e (or (em.kind= e :str) (not (atom? e))))
        (case e
          [:str s] (if (= n (length pos))
                       (s:sub (. pos n) (. pos n)))
          [head & items] (if (= n (length pos))
                             (. items (. pos n))
                             (at-impl (. items (. pos n)) pos (+ n 1))))))
  (if-signature e 2 :inf
                (let [pos (fcollect [i 3 (length e)]
                            (case (. e i)
                              [:int n] n
                              _ nil))
                      val (case (em.arg e 1)
                            [:hold val] val
                            any any)]
                  (or (if (= (- (em.args# e) 1) (length pos))
                          (at-impl val pos 1)
                          nil)
                      e))))

(fn expect-signature [e sigtab]
  (assert (= (em.args# e) (length sigtab))
          (string.format "expected %d arguments" (length sigtab)))
  (each [i v (ipairs sigtab)]
    (assert (or (= v :any) (= (em.kind (. e (+ 1 i))) v))
            (string.format "expected argument %d of type %s" i v))))

(fn simplify.kind [e]
  (expect-signature e [:any])
  [:str (or (em.kind (em.arg e 1)) :unknown)])

(fn simplify.isa [e]
  (expect-signature e [:any :str])
  (let [[kind & _] (em.arg e 1)
        [_      b] (em.arg e 2)]
    [:int (or (and (= kind b)))]))

(fn simplify.call [e env]
  (case (em.kind e)
    "cons"  (simplify.cons e false)
    "rcons" (simplify.cons e true)
    "apply" (simplify.apply e env)
    "table" (simplify.table e env)
    "length" (simplify.length e)
    "nth" (simplify.nth e)
    "kind" (simplify.kind e)
    "isa" (simplify.isa e)
    _ e))

(macro relation-op? [s]
  `(case ,s
    "<" 1 "<=" 1 "=" 1 "!=" 1 ">" 1 ">=" 1
    _# false))

(fn compare-op [e]
  (let [op (em.kind e)
        (f cmp-res) (case op
            "="  (values calc.eq 1)
            "!=" (values calc.not-eq 0)
            "<"  (values calc.lt 0)
            "<=" (values calc.lt-eq 1)
            ">"  (values calc.gt 0)
            ">=" (values calc.gt-eq 1))
        a (em.arg e 1)
        b (em.arg e 2)]
    (assert f)
    (if (and (calc.number? a) (calc.number? b))
        (f a b)
        (order.compare a b)
        [:int cmp-res]
        e)))

(fn simplify.relation [e env]
  (trace.step "relation" e)
  (let [op (em.kind e) a (em.arg e 1) b (em.arg e 2)]
    (if (and (relation-op? (em.kind a))
             (not (relation-op? (em.kind b))))
        (let [ab (em.arg a 2)]
          (simplify.simplify [:and (simplify.simplify a env)
                                   (simplify.simplify [op ab b] env)] env))
        (let [e (simplify.simplify-rec e env)]
          (compare-op e)))))

(fn simplify.logic-and [e]
  (let [a (em.arg e 1)
        b (em.arg e 2)]
    (trace.step "logic-and" a b)
    (if (and (calc.number? a) (calc.number? b))
        (calc.logic-and a b)
        (and (calc.zero? a))
        [:int 0]
        (and (calc.zero? b))
        [:int 0]
        e)))

(fn simplify.logic-or [e]
  (let [a (em.arg e 1)
        b (em.arg e 2)]
    (trace.step "logic-or" a b)
    (if (and (calc.number? a)
             (calc.number? b))
        (calc.logic-or a b)
        (or (and (calc.number? a) (not (calc.zero? a)))
            (and (calc.number? b) (not (calc.zero? b))))
        [:int 1]
        e)))

(fn simplify.logic-not [e]
  (trace.step "logic-not" e)
  (let [a (em.arg e 1)]
    (if (em.kind= a :not)
        (simplify.logic-not (em.arg a 1))
        (calc.number? a)
        [:int (or (and (calc.zero? a) 1) 0)]
        e)))

(fn simplify.apply-rules [e env name]
  "Apply rules to expression."
  (let [rule (require :rule)
        list (env:get-rules name)]
    (if list
        (rule.apply e env list)
        e)))

(fn simplify.chaining [e env]
  (fn put-front [a b]
    (let [e b]
      (table.insert e 2 a)
      e))

  (fn put-back [a b]
    (let [e b]
      (table.insert e a)
      e))

  (let [a (simplify.simplify (em.arg e 1) env)
        b (em.arg e 2)]
    (trace.step "chaining" a b)
    (assert (not (. operators (em.kind b))) "right hand side must not be an operator")
    (if (not (atom? b))
        (simplify.simplify (case (em.kind e)
                             "|>"  (put-front a b)
                             "|>>" (put-back a b)
                             _ e) env)
        e)))

(lambda simplify.simplify [e env]
  (if (or (em.kind= e :int)
          (em.kind= e :frac)
          (em.kind= e :real))
      (simplify.rational-number e)
      (or (em.kind= e :ident)
          (em.kind= e :str)
          (em.kind= e :quote))
      e
      (em.kind= e "hold")
      e
      (relation-op? (em.kind e))
      (simplify.relation e env)
      (or (em.kind= e "|>")
          (em.kind= e "|>>"))
      (simplify.chaining e env)
      (let [e (simplify.simplify-rec e env)]
        (case (em.kind e)
          "^" (simplify.power e)
          "*" (simplify.product e)
          "+" (simplify.sum e)
          "/" (simplify.quotient e)
          "-" (simplify.difference e)
          "and" (simplify.logic-and e)
          "or" (simplify.logic-or e)
          "not" (simplify.logic-not e)
          s   (simplify.call e env)
          _ e))))

(lambda simplify.full [e env]
  "Fully simplify expression E including apllying rules."
  (-> e
      (simplify.apply-rules env :pre-simplify)
      (simplify.simplify env)
      (simplify.apply-rules env :simplify)))

simplify
