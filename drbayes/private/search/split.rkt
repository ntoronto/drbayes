#lang typed/racket/base

(require racket/list
         "../set.rkt"
         "../flonum.rkt")

(provide (all-defined-out))

(: interval-split (-> Nonempty-Prob-Interval (Listof Nonempty-Prob-Interval)))
(define (interval-split I)
  (define-values (a b a? b?) (prob-interval-fields I))
  (define c (prob-midpoint a b))
  (define m1 (prob- c a))
  (define m2 (prob- b c))
  (cond [(and (prob? m1) (not (prob-0? m1))
              (prob? m2) (not (prob-0? m2)))
         (define I1 (prob-interval a c a? #t))
         (define I2 (prob-interval c b #f b?))
         (cond [(and (not (empty-prob-set? I1)) (prob-interval-can-sample? I1)
                     (not (empty-prob-set? I2)) (prob-interval-can-sample? I2))
                (list I1 I2)]
               [else  empty])]
        [else  empty]))
