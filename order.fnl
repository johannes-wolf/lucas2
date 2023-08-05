(local order {})
(local calc (require :calc))
(local alg (require :alg))
(local expr (require :expr))

(fn order.compare [a b]
  (if (or (not a) (not b))
      (and (not a) (not b))
      (and (not= (type a) :table) (not= (type b) :table))
      (= a b)
      (and (calc.number? a) (calc.number? b))
      (= (calc.compare-num a b) 0)
      (and (= (. a 1) (. b 1)) (= (length a) (length b))
           (do (var res? true)
               (for [i 2 (length a)] :until (not= res? true)
                    (set res? (order.compare (. a i) (. b i))))
               res?))))

(macro not-in= [s ...]
  (or (each [_ v (ipairs [...])]
        `(not= s v))))

(macro underscore-ident [s]
  `(= (string.sub ,s 1 1) "_"))

(fn order.ident [u v]
  (let [su (. u 2) sv (. v 2)]
    (if (and (underscore-ident su) (underscore-ident sv))
        (< su sv)
        (underscore-ident su)
        false
        (underscore-ident sv)
        true
        (< su sv))))

(fn order.front [u v]
  (let [uk (. u 1) vk (. v 1)]
    (if (and (calc.number? u) (calc.number? v))
        (< (calc.compare-num u v) 0)
        (= uk vk)
        (case uk
          "ident" (order.ident u v)
          "*" (order.sum-prod u v)
          "+" (order.sum-prod u v)
          "^" (order.power u v)
          "!" (order.front (. u 2) (. v 2))
          s (order.call u v)
          _ false)
        (and (calc.const? u) (not (calc.const? v)))
        true
        (and (= uk "*") (not-in= vk "*"))
        (order.front u ["*" v])
        (and (= uk "^") (not-in= vk "*" "^"))
        (order.front u ["^" v [:int 1]])
        (and (= uk "+") (not-in= vk "*" "^" "+"))
        (order.front u ["+" v])
        (= uk "ident")
        (not (underscore-ident (. u 2)))
        (= vk "ident")
        (underscore-ident (. v 2))
        (not (order.front v u))
        )))

(fn backwards-compare-args [a b]
  (let [la (length a) lb (length b)]
  (var res? nil)
  (for [j 1 (- (math.min (length a) (length b)))] :until (not= res? nil)
       (if (not (order.compare (. a (- la j)) (. b (- lb j))))
           (set res? (order.front (. a (- la j)) (. b (- lb j)))))
  res?)))

(fn order.sum-prod [a b]
  (let [la (length a)
        lb (length b)]
    (if (not= la lb)
        (order.front (. a la) (. b lb))
        (do (var res? (backwards-compare-args a b))
            (if (not res?)
                (let [k (- (math.min la lb) 1)]
                  (if (order.compare (. a (- la k)) (. b (- lb k)))
                      (set res? (< la lb)))))
            (or res? false)))))

(fn order.call [u v]
  false) ; TODO

(fn order.power [u v]
  (if (not (order.compare (alg.base u) (alg.base v)))
      (order.front (alg.base u) (alg.base v))
      (order.front (alg.expontent u) (alg.expontent v))))

order
