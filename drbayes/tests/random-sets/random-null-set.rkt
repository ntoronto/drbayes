#lang typed/racket/base

(require drbayes/private/set)

(provide (all-defined-out))

(: random-null-set (-> Null-Set))
(define (random-null-set)
  (if ((random) . < . 0.5) empty-null-set nulls))

(: random-null (Null-Set -> Null))
(define (random-null A)
  (cond [(empty-null-set? A)  (raise-argument-error 'random-null "Full-Null-Set" A)]
        [else  null]))
