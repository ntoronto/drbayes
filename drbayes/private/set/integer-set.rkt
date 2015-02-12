#lang typed/racket/base

(require "ordered-set.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(define-singleton-type -Inf -inf)
(define-singleton-type +Inf +inf)

(define-type Extended-Integer (U -Inf +Inf Integer))

(: int< (-> Extended-Integer Extended-Integer Boolean))
(define (int< a b)
  (cond [(or (+inf? a) (-inf? b))  #f]
        [(or (-inf? a) (+inf? b))  #t]
        [else  (< a b)]))

(define-ordered-set
  #:names Integer integer
  #:types Extended-Integer
  #:predicates
  (λ (x) #t)
  exact-integer?
  -inf?
  +inf?
  #:comparisons equal? int<
  #:guards
  (λ (a b a? b?) (or (+inf? a)
                     (-inf? b)
                     (and (integer? a) (integer? b) (= 1 (- b a)) (not a?) (not b?))))
  (λ (a b a? b?) (and (-inf? a) (+inf? b)))
  (λ (a b a? b?) (values a b (if (-inf? a) #f a?) (if (+inf? b) #f b?)))
  )

(: integer->singleton (-> Integer Plain-Integer-Interval))
(define (integer->singleton x)
  (plain-integer-interval x x #t #t))

(: integer-interval-fields (-> Nonempty-Integer-Interval
                               (Values Extended-Integer Extended-Integer Boolean Boolean)))
(define (integer-interval-fields I)
  (if (integers? I)
      (values -inf +inf #f #f)
      (values (Plain-Integer-Interval-min I)
              (Plain-Integer-Interval-max I)
              (Plain-Integer-Interval-min? I)
              (Plain-Integer-Interval-max? I))))
