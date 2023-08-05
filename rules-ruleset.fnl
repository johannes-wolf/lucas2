(local list (require :list))
(local rule (require :rule))
(import-macros em :macros)

(local environ (require :env))
(local env environ.global)

;; Table of all user defined rulesets
(local user-rulesets {})

(fn addrule [{: name : p : t}]
  (let [name (or name [:str :simplify])]
    (assert (em.kind= name :str) "name must be of type string")
    (env:put-rules (em.arg name 1) [(rule.new p t)])
    [:list [:hold p] [:hold t]]))

(fn listrules [{: name}]
  (assert (em.kind= name :str) "name must be of type string")
  [:list (table.unpack (icollect [_ v (ipairs (or (env:get-rules (em.arg name 1)) []))]
                         [:list [:hold v.pattern]
                          (if (= (type v.replacement) :function)
                              [:str :luafn]
                              [:hold v.replacement])]))])

(let [r [(rule.new "AddRule(p,t)" addrule)
         (rule.new "AddRule(name,p,t)" addrule)
         (rule.new "ListRules(name)" listrules)]]
  (env:put-rules :simplify r))
(env:set-attribute "addrule" :hold :rest)
