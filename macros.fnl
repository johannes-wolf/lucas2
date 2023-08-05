;; This file contains macros for use with expressions
(local em {})

(fn em.kind [e]
  (assert (= (type e) :table))
  `(. ,e 1))

(fn em.kind= [e k]
  `(= (. ,e 1) ,k))

(fn em.args# [e]
  (assert (= (type e) :table))
  `(- (length ,e) 1))

(fn em.arg [e n]
  (assert (= (type e) :table))
  (assert (= (type n) :number))
  `(. ,e (+ ,n 1)))

(fn em.int= [e n]
  (assert (= (type n) :number))
  `(match ,e
     [:int ,n] true
     _# false))

(fn em.int [n]
  (assert (= (type n) :number))
  `[:int ,n])

(fn em.frac [n d]
  (assert (= (type n) :number))
  (assert (and (= (type d) :number) (not= d 0)))
  `[:frac ,n ,d])

(fn em.real [f]
  (assert (= (type f) :number))
  `[:real ,f])

em
