(local frac (require :frac))
(local calc {})
(import-macros em :macros)

(macro get-real [n]
  `(. ,n 2))

(macro typeof [e]
  `(. ,e 1))

(fn calc.integer? [e]
  (em.kind= e :int))

(fn calc.fraction? [e]
  (em.kind= e :frac))

(fn calc.real? [e]
  (em.kind= e :real))

(fn calc.string? [e]
  (em.kind= e :str))

(fn calc.number? [e]
  (or (calc.integer? e)
      (calc.fraction? e)
      (calc.real? e)))

(fn calc.const? [e]
  (or (calc.number? e)
      (calc.string? e)))

(fn calc.symbol? [e]
  "Returns if E is a symbol."
  (em.kind= e :ident))

(fn calc.zero? [e]
  "Returns true if E is exqual to zero."
  (= 0 (case e
         [:int n] n
         [:frac n _] n
         [:real n] n)))

(fn calc.inf? [e]
  "Returns if E is positive or negative infinity."
  (case e
    [:ident :inf] 1
    [:- [:ident :inf]] -1))

(fn calc.nan? [e]
  "Returns if E is symbol nan."
  (case e
    [:ident :nan] 1))

(fn calc.nil? [e]
  "Returs if E is nil."
  (case e
    [:ident :nil] true
    nil true
    _ false))

(fn bool->int [b]
  (if (= b true)
      [:int 1]
      (= b false)
      [:int 0]
      b))

(fn int->frac [n]
  "Convert N to fraction if its type is integer."
  (case n
    [:int n] [:frac n 1]
    _ n))

(fn frac->real [n]
  "Convert N to real if its type is fraction."
  (case n
    [:frac num denom] [:real (/ num denom)]
    _ n))

(fn handle2 [a b handle-fraction handle-real]
  "Handle two arguments converted to :frac or :real."
  (let [a (int->frac a) type-a (. a 1)
        b (int->frac b) type-b (. b 1)]
    (if (or (= type-a :real) (= type-b :real))
        (let [a (frac->real a)
              b (frac->real b)]
          (handle-real (get-real a) (get-real b)))
        (and (= type-a :frac) (= type-b :frac))
        (let [(a b) (frac.expand a b)]
          (handle-fraction a b)))))

(fn calc.compare-num [a b]
  "Compares two values A and B numerically. Returns 0 if both
   are equal <0 if A is less, >0 if more. Arguments must be
   normalized."
  (let [a (int->frac a)
        b (int->frac b)]
    (if (and (calc.fraction? a) (calc.fraction? b))
        (if (< (. a 2) (. b 2)) -1
            (> (. a 2) (. b 2))  1
            0)
        (let [a (frac->real a)
              b (frac->real b)]
          (if (and (calc.real? a) (calc.real? b))
              (if (< (. a 2) (. b 2)) -1
                  (> (. a 2) (. b 2))  1
                  0))))))

(fn calc.neg [a]
  (let [a (int->frac a) type-a (. a 1)]
    (if (= type-a :real)
        [:real (* -1 (get-real a))]
        (= type-a :frac)
        (frac.new (* -1 (frac.num a)) (frac.denom a)))))

(fn calc.add [a b]
  (handle2 a b
           #(frac.new (+ (frac.num $1) (frac.num $2))
                      (frac.denom $1))
           #[:real (+ $1 $2)]))

(fn calc.mul [a b]
  (handle2 a b
           #(frac.new (* (frac.num $1) (frac.num $2))
                      (* (frac.denom $1) (frac.denom $2)))
           #[:real (* $1 $2)]))

(fn calc.div [a b]
  (handle2 a b
           #(frac.new (* (frac.num $1) (frac.denom $2))
                      (* (frac.num $2) (frac.denom $1)))
           #[:real (/ $1 $2)]))

(fn pow-int-int [a b]
  (if (not= a 0)
      (if (> b 0)
          (calc.mul (pow-int-int a (- b 1)) [:int a])
          (= b 0)
          [:int 1]
          (= b -1)
          (calc.div [:int 1] [:int a])
          (< b -1)
          (calc.div [:int 1] (pow-int-int a (* -1 b))))
      (if (>= b 1)
          [:int 0]
          [:nan])))

(fn calc.pow [a b]
  (handle2 a b
           #(if (and (= (frac.denom $1) 1) (= (frac.denom $2) 1))
                (pow-int-int (frac.num $1) (frac.num $2))
                (= (. (frac.normalized $2) 1) :int)
                (let [b (. (frac.normalized $2) 2)]
                  (calc.div (pow-int-int (frac.num $1) b)
                            (pow-int-int (frac.denom $1) b)))
                [:real (^ (get-real (frac->real $1)) (get-real (frac->real $2)))])
           #[:real (^ $1 $2)]))

(fn calc.eq [a b]
  (bool->int (if (and (calc.number? a) (calc.number? b))
                 (= (calc.compare-num a b) 0))))

(fn calc.not-eq [a b]
  (bool->int (if (and (calc.number? a) (calc.number? b))
                 (not= (calc.compare-num a b) 0))))

(fn calc.gt [a b]
  (bool->int (if (and (calc.number? a) (calc.number? b))
                 (> (calc.compare-num a b) 0))))

(fn calc.gt-eq [a b]
  (bool->int (if (and (calc.number? a) (calc.number? b))
                 (>= (calc.compare-num a b) 0))))

(fn calc.lt [a b]
  (bool->int (if (and (calc.number? a) (calc.number? b))
                 (< (calc.compare-num a b) 0))))

(fn calc.lt-eq [a b]
  (bool->int (if (and (calc.number? a) (calc.number? b))
                 (<= (calc.compare-num a b) 0))))

(fn calc.logic-and [a b]
  (bool->int (if (or (calc.nil? a) (calc.nil? b))
                 false
                 (and (calc.number? a) (calc.number? b))
                 (or (and (not (calc.zero? a)) (not (calc.zero? b)) true) false))))

(fn calc.logic-or [a b]
  (bool->int (if (or (calc.nil? a) (calc.nil? b))
                 false
                 (and (calc.number? a) (not (calc.zero? a))) true
                 (and (calc.number? b) (not (calc.zero? b))) true
                 (and (calc.number? a) (calc.number? b)) false)))

(macro wrap-a [f]
  `(lambda [env#] (,f (. env# :a))))

(macro wrap-ab [f]
  `(lambda [env#] (,f (. env# :a) (. env# :b))))

;(tset calc :rules
;       [(rule.new "-a" (wrap-a calc.neg))
;        (rule.new "-a"  "-1*a")
;        (rule.new "a-b" "a+(-b)")
;        
;        (rule.new "0+a" "a")
;        (rule.new "a+b" (wrap-ab calc.add))
;
;        (rule.new "0*a" "0")
;        (rule.new "1*a" "a")
;        (rule.new "a*b" (wrap-ab calc.mul))
;        ;(rule.new "a*b :: base(a)=base(b)"
;        ;          "base(a)^(exponent(a)+exponent(b))")
;
;        (rule.new "a^inf" "inf")
;        (rule.new "a^-inf" "-inf")
;        (rule.new "a^0" "1")
;        (rule.new "a^1" "a")
;        (rule.new "a^b" (wrap-ab calc.pow))
;        (rule.new "e^a" "exp(a)")
;
;        (rule.new "a! :: a=0" "1")
;        (rule.new "a! :: a>0" "a*(a-1)!")
;
;        (rule.new "a=a" "1")
;        (rule.new "a=b" (wrap-ab calc.eq))
;        (rule.new "a!=a" "0")
;        (rule.new "a!=b" (wrap-ab calc.not-eq))
;        (rule.new "a<a" "0")
;        (rule.new "a<b" (wrap-ab calc.lt))
;        (rule.new "a>a" "0")
;        (rule.new "a>b" (wrap-ab calc.gt))
;        (rule.new "a<=a" "1")
;        (rule.new "a<=b" (wrap-ab calc.lt-eq))
;        (rule.new "a>=a" "1")
;        (rule.new "a>=b" (wrap-ab calc.gt-eq))
;
;        (rule.new "abs(a) :: a < 0" "-1*a")
;        (rule.new "abs(a) :: a >= 0" "a")
;
;        (rule.new "string?(a)" (wrap-a #(bool->int (calc.string? $1))))
;        (rule.new "number?(a)" (wrap-a #(bool->int (calc.number? $1))))
;        (rule.new "symbol?(a)" (wrap-a #(bool->int (calc.symbol? $1))))
;
;        ;(rule.new "base(a)" (wrap-a calc.base))
;        ;(rule.new "exponent(a)" (wrap-a calc.expontent))
;        ;(rule.new "term(a)" term)
;        ;(rule.new "factor(a)" constant-factor)
;
;        (rule.new "bool(a)" "a!=0")
;        (rule.new "not 0" "1")
;        (rule.new "not a :: a!=0" "0")
;        (rule.new "a and a" "bool(a)")
;        (rule.new "a and b" (wrap-ab calc.logic-and))
;        (rule.new "a or a" "bool(a)")
;        (rule.new "a or b" (wrap-ab calc.logic-or))
;        ])

calc
