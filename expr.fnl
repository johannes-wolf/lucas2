(local parselets (require :parselets))
(local frac (require :frac))

(local expr {})
(tset expr :__index expr)

(fn expr.compile [str]
  "Compile expression STR."
  (let [scanner (require :scanner)
        parser (require :parser)
        t (scanner.tokenize str)
        p (parser.new parselets t)]
    (p:parse)))

(fn expr.true? [e]
  "Returns if expression E is considered true."
  (case e
    [:int v] (not= v 0)
    [:frac n _] (not= n 0)
    [:real v] (not= v 0)
    _ false))

(fn expr.false? [e]
  "Returns if expression E is false."
  (not (expr.true? e)))

(fn expr.to-string [e]
  "Convert expression to string"
  (case e
    [:nan] :nan
    [:inf] :inf
    [:int i] (tostring i)
    [:real i] (tostring i)
    [:frac & _] (table.concat (icollect [i v (ipairs (frac.split-3 e))]
                                (when (not (and (= i 1) (= v 0)))
                                  (tostring v))) ":")
    [:ident s] s
    [:str s] s
    [head & _rest] (string.format "(%s %s)"
                                  head
                                  (table.concat (icollect [_ v (ipairs _rest)]
                                                  (expr.to-string v)) " "))
    value (tostring value)))


expr
