(local frac {})

(fn sgcd [a b]
  "Find GCD for A and B."
  (or (and (= b 0) a) (sgcd b (% a b))))

(fn frac.new [num ?denom ?normalize]
  "Make a new fraction"
  (if (= ?normalize false)
      [:frac num (or ?denom 1)]
      (frac.normalize num (or ?denom 1))))

(fn frac.expand [a b]
  "Return both fractions multiplied by each others denominator."
  (if (= (frac.denom a) (frac.denom b))
      (values a b)
      (let [denom (* (frac.denom a) (frac.denom b))]
        (values (frac.new (* (frac.num a) (frac.denom b)) denom false)
                (frac.new (* (frac.num b) (frac.denom a)) denom false)))))

(fn frac.normalized [f]
  "Return normalized fraction F."
  (frac.normalize (frac.num f) (frac.denom f)))

(fn frac.normalize [num denom]
  "Return normalized fraction. This function can return an int expression,
   if the resulting fraction would have denominator 1."
  (let [(num denom) (if (< denom 0)
                        (values (* -1 num) (* -1 denom))
                        (values num denom))
        gcd (sgcd num denom)]
    (if (= gcd 1)
        (if (= denom 1)
            [:int (math.floor num)]
            [:frac num denom])
        (= gcd denom)
        [:int (math.floor (/ num denom))]
        ;; Else
        [:frac (math.floor (/ num gcd)) (math.floor (/ denom gcd))])))

(fn frac.split [n]
  "Split a fraction into numerator and denominator."
  (values (. n 2) (. n 3)))

(fn frac.num [n]
  "Get fraction numerator."
  (. n 2))

(fn frac.denom [n]
  "Get fraction denominator."
  (. n 3))

(fn frac.split-3 [n]
  "Split fraction into integer, numerator and denominator parts."
  (let [(num denom) (frac.split n)
        ipart (math.floor (/ num denom))]
    (if (>= num denom)
        [ipart (- num (* ipart denom)) denom]
        [0 num denom])))

frac
