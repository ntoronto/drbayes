#lang typed/racket/base

(require racket/match
         math/flonum
         "ordered-set.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(define-ordered-set
  #:names Real real
  #:types Flonum
  #:predicates
  (λ (x) (<= -inf.0 x +inf.0))
  (λ (x) (< -inf.0 x +inf.0))
  (λ (x) (= x -inf.0))
  (λ (x) (= x +inf.0))
  #:comparisons = <
  #:guards
  (λ (a b a? b?) (or (= a +inf.0) (= b -inf.0)))
  (λ (a b a? b?) (and (= a -inf.0) (= b +inf.0)))
  (λ (a b a? b?)
    (values (if (= a 0.0) +0.0 a)
            (if (= b 0.0) -0.0 b)
            (if (= a -inf.0) #f a?)
            (if (= b +inf.0) #f b?)))
  )

(define positive-interval (plain-real-interval 0.0 +inf.0 #f #f))
(define negative-interval (plain-real-interval -inf.0 -0.0 #f #f))
(define nonnegative-interval (plain-real-interval 0.0 +inf.0 #t #f))
(define nonpositive-interval (plain-real-interval -inf.0 -0.0 #f #t))
(define zero-interval (plain-real-interval +0.0 -0.0 #t #t))
(define unit-interval (plain-real-interval 0.0 1.0 #t #t))

(: flonum->singleton (-> Flonum Plain-Real-Interval))
(define (flonum->singleton x)
  (cond [(< -inf.0 x +inf.0)  (plain-real-interval x x #t #t)]
        [else  (raise-argument-error 'flonum->singleton "rational Flonum" x)]))

(: real-interval-fields (-> Nonempty-Real-Interval (Values Flonum Flonum Boolean Boolean)))
(define (real-interval-fields I)
  (if (reals? I)
      (values -inf.0 +inf.0 #f #f)
      (values (Plain-Real-Interval-min I)
              (Plain-Real-Interval-max I)
              (Plain-Real-Interval-min? I)
              (Plain-Real-Interval-max? I))))
