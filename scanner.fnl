(var operators (icollect [k _ (pairs (require :operators))] k))
(table.sort operators (lambda [a b] (> (length a) (length b))))
(set operators (icollect [_ v (ipairs operators)]
                 (let [v (v:gsub "[%%%*%+%-%|]" (lambda [s] (.. "%" s)))]
                   (if (v:find ".*%w$")
                       (.. "^(" v ")[^%w_%.]") ; FIXME: this is bad! Use a token tree instead of regexp!
                       (.. "^(" v ")")))))

(let [scanner {}]
  (fn skip-ws [str pos]
    "Skip whitespace in STR at POS. Returns new position."
    (str:find "^%s+" pos))

  (fn parse-syntax [str pos]
    "Parse one single syntax symbol at POS."
    (str:find "^([%[%](){};,:])" pos))

  (fn parse-ident [str pos]
    "Parse identifier at POS."
    (str:find "^(&?[%a_][%w_?]*)" pos))

  (fn parse-lisp-string [str pos]
    "Parse a lisp style string."
    (str:find "^:([%a][%w-_/%.]+)" pos))

  ; TODO: Support escape character
  (fn parse-string-literal [str pos]
    "Parse string literal at POS."
    (let [(ii jj tt) (str:find "^\"(.*)\"" pos)]
      (when tt
        (values ii jj (tt:sub 1 -1)))))

  (fn parse-operator [str pos]
    "Parse operator at POS."
    (var done? nil)
    (each [_ re (ipairs operators)] :until done?
          (let [(i j t) (str:find re pos)]
            (when (and i (not done?))
              (set done? [i j t]))))
    (if done?
        (values (table.unpack done?))
        nil))

  (fn parse-integer [str pos]
    (str:find "^(%d+)" pos))

  (fn parse-real [str pos]
    (str:find "^(%d*%.%d+)" pos))

  (fn scanner.tokenize [str]
    "Tokenize input into list of tokens."
    (let [j (length str)
          scanners [[:ws    skip-ws]
                    [:op    parse-operator]
                    [:real  parse-real]
                    [:int   parse-integer]
                    [:ident parse-ident]
                    [:str   parse-lisp-string]
                    [:str   parse-string-literal]
                    [:syn   parse-syntax]]]
      (var t [])
      (var i 1)
      (while (<= i j)
        (var break? false)
        (let [prev-i i]
          (each [idx v (ipairs scanners) :until break?]
            (let [kind        (. v 1)
                  scan-fn     (. v 2)
                  (_ jj text) (scan-fn str i)]
              (when jj
                (set break? true)
                (set i (+ jj 1)))
              (when text
                (table.insert t {:text text :kind kind}))))
          (when (= i prev-i)
            (error (string.format "Unexpected input at %d: %s" i (str:sub i))))))
      t))

  scanner)
