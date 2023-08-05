(local list {})

(fn list.slice [l start? end?]
  "Return slice of list from START? to END?."
  (fcollect [i (or start? 1) (or end? (length l))]
    (. l i)))

(fn list.head [l]
  "Get head of list."
  (if (> (length l) 0)
      (. l 1)
      nil))

(fn list.tail [l]
  "Get tail of list."
  (if (> (length l) 1)
      (list.slice l 2 nil)
      []))

(fn list.map [l f begin? end?]
  "Call function on range of list."
  (for [i (or begin? 1) (or end? (length l))]
    (tset l i (f (. l i))))
  l)

(fn list.find [l what cmp?]
  "Find WHAT in list using comparator CMP."
  (let [cmp (or cmp? #(= what $1))]
    (var found? nil)
    (each [_ v (ipairs l)] :until (not= found? nil)
          (set found? (and (cmp? v) v)))
    found?))

(fn list.copy [source target begin? end?]
  "Copy range from SOURCE into TARGET."
  (let [target (or target [])]
    (for [i (or begin? 1) (or end? (length source))]
      (table.insert target (. source i))))
  target)

(fn list.prepend [a b]
  "Prepend value A to list B."
  (table.insert b 1 a)
  b)

(fn list.append [a b]
  "Append value A to list B."
  (table.insert b a)
  b)

(fn list.append-list [a b]
  "Append items of B to list A."
  (each [_ v (ipairs b)]
    (table.insert a v))
  a)

(fn list.join [a b]
  "Join two lists A and B."
  (let [new []]
    (list.copy a new)
    (list.copy b new)
    new))

list
