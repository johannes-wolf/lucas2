(local operators (require :operators))
(local frac (require :frac))

(fn parse-ident [p token]
  (if (p:match :syn "(")
      (do (p:consume)
        [token.text (table.unpack (p:parse-list {:kind :syn :text ","}
                                                {:kind :syn :text ")"}))])
      [:ident token.text]))

(fn parse-string [_ token]
  "Parse a string literal."
  [:str token.text])

(fn parse-fraction-abc [p a]
  "Parse fraction in [a:]b:c form."
  (let [b (tonumber (. (p:expect :int) :text))]
    (if (p:match :syn ":")
        (do (p:consume)
            (let [c (tonumber (. (p:expect :int) :text))]
              (frac.new (+ (* a c) b) c)))
        (frac.new a b))))

(fn parse-int-or-fraction [p token]
  "Parse integer or fraction."
  (if (p:match :syn ":")
      (do (p:consume)
          (parse-fraction-abc p (tonumber token.text)))
      [:int (tonumber token.text)]))

(fn parse-real [_ token]
  "Parse real value."
  [:real (tonumber token.text)])

(fn parse-parentheses [p token]
  "Parse an expression enclosed in parentheses."
  (let [e (p:parse)]
    (p:expect :syn ")")
    e))

(fn parse-list [p token]
  "Parse a list of comma separated values."
  [:list (table.unpack (p:parse-list {:kind :syn :text ","}
                                     {:kind :syn :text "]"}))])

(fn parse-statement [p left _] ;TODO: Remove?
  "Parse a list of statements."
  (var l [:stmt left])
  (var done? false)
  (while (and (not (p:eof-p)) (not done?))
    (table.insert l (p:parse))
    (when (not (p:match :syn ";"))
      (set done? true)))
  l)

(local parselets {"ident" {:prefix parse-ident}
                  "str"   {:prefix parse-string}
                  "int"   {:prefix parse-int-or-fraction}
                  "real"  {:prefix parse-real}
                  "("     {:prefix parse-parentheses}
                  "["     {:prefix parse-list}
                  ";"     {:infix  parse-statement :precedence 1}})

;; Register operators
(each [k v (pairs (require :operators))]
  (var parselet {:precedence (or v.infix v.suffix)})
  (when v.prefix
    (tset parselet :prefix (lambda [p _] [k (p:parse-precedence v.prefix)])))
  (when v.infix
    (tset parselet :infix (lambda [p left _] [k left (p:parse-precedence v.infix)])))
  (when v.suffix
    (tset parselet :infix (lambda [p left _] (p:parse-infix [k left] 0))))
  (tset parselets k parselet))

;; Register implicit multiplication
(fn parse-implicit-mul [p left token]
  (let [k (or (and (or (= token.kind :syn) (= token.kind :op))
                   token.text) token.kind)
        right (p:parse-infix ((. (. parselets k) :prefix) p token)
                             (. (. parselets "*") :precedence))]
    ["*" left right]))

(fn implicit-mul [kind]
  (tset (. parselets kind) :precedence (. (. parselets "*") :precedence))
  (tset (. parselets kind) :infix parse-implicit-mul))

(implicit-mul "ident")
(implicit-mul "int")
(implicit-mul "real")
(implicit-mul "(")

parselets
