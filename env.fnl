(local list (require :list))

(local env {})
(tset env :__index env)

(local expr (require :expr))

(fn env.new [?parent]
  "Create a empty environment with ?PARENT."
  (setmetatable {:table {} :rules {} :parent ?parent} env))

(fn env._put [self key value]
  "Put KEY VALUE pair into environment."
  (tset self.table key value))

(fn env._get [self key]
  "Retreive value for KEY."
  (let [value  (. self.table key)
        parent (. self :parent)]
    (if value
        value
        (when parent
          (parent:_get key)))))

(fn env.put-rules [self name list]
  "Put LIST of rules into ruleset with NAME."
  (if (not (. self.rules name))
      (tset self.rules name (or list []))
      (let [rs (. self.rules name)]
        (each [_ r (ipairs list)]
          (table.insert rs r))))
  self)

(fn env.get-rules [self name]
  "Get active list of rules for NAME."
  (if self.parent
      (let [l (self.parent:get-rules name)]
        (each [_ r (ipairs (. self.rules name))]
          (table.insert l r)))
      (. self.rules name)))

(fn env.get-rulesets [self]
  "Get a list of ruleset names."
  (let [l (if self.parent (self.parent:get-rulesets) [])]
    (list.append-list l (collect [k _ (pairs self.rules)] k))
    l))

(fn env.get-attribute [self symbol attrib ?default]
  "Retreive attribute of SYMBOL for key ATTRIB.
   Attributes are just fancy variables whose values are lua values."
  (let [value (self:_get (.. symbol "/" attrib))]
    (if (not= value nil)
        value
        ?default)))

(fn env.set-attribute [self symbol attrib value]
  "Set attribute of SYMBOL for key ATTRIB to VALUE."
  (self:_put (.. symbol "/" attrib) value)
  self)

(fn env.print [self]
  "Print SELF to stdout."
  (when self.parent
    (self.parent:print)
    (print "---"))
  (each [k v (pairs self.table)]
    (print (string.format "%s: %s" k (expr.to-string v)))))

;; Instantiate global environment
(tset env :global (env.new))

env
