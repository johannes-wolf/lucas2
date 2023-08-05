(local list (require :list))
(local calc (require :calc))
(import-macros em :macros)

(local alg {})
(tset alg :zero (em.int 0))
(tset alg :one  (em.int 1))

(fn alg.term [e]
  "Get non-const term of expression."
  (if (calc.const? e) nil
      (em.kind= e "*")
      (if (calc.const? (em.arg e 1))
          ["*" (table.unpack (list.slice e 3))]
          e)
      ["*" e]))

(fn alg.const [e]
  "Returns the constant factor of expression e."
  (if (calc.const? e) nil
      (em.kind= e "*")
      (if (calc.const? (em.arg e 1))
          (em.arg e 1)
          alg.one)
      alg.one))

(fn alg.base [a]
  (case a
    ["^" base _] base
    _ a))

(fn alg.exponent [a]
  (case a
    ["^" _ expo] expo
    _ alg.one))

alg
