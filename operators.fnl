(let [operators {":="  {:infix 1}
                 ":==" {:infix 1}

                 "|>"  {:infix 2}
                 "|>>" {:infix 2}

                 "::"  {:infix 2}
                 "|"   {:infix 2}

                 "or"  {:infix 3}
                 "and" {:infix 4}
                 "not" {:prefix 5}

                 "="   {:infix 6}
                 "!="  {:infix 6}

                 "<="  {:infix 8}
                 "<"   {:infix 8}
                 ">="  {:infix 8}
                 ">"   {:infix 8}

                 "+"   {:infix 9}
                 "-"   {:infix 9 :prefix 11}
                 "*"   {:infix 10}
                 "/"   {:infix 10}
                 "^"   {:infix 12}
                 "!"   {:suffix 13}}]
  operators)
