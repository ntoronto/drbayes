#lang typed/racket/base

(require "../flonum.rkt")

(provide (all-defined-out))

(define prob-0.5 (Prob (log 0.5)))

(: prob-renormalize-first (-> Prob Prob Prob))
;; Assumes p1 and p2 sum to less than 1, modulo floating-point error
(define (prob-renormalize-first p1 p2)
  (cond [(prob-0? p1)  prob-0]
        [(prob-0? p2)  prob-1]
        [else
         (define p (prob+ p1 p2))
         (cond [(or (bad-prob? p) (prob-1? p))  p1]
               [else
                (define q1 (prob/ p1 p))
                (if (prob? q1) q1 prob-1)])]))

(: prob-normalize-first (-> Prob Prob Prob))
(define (prob-normalize-first p1 p2)
  (prob-renormalize-first (prob* p1 prob-0.5)
                          (prob* p2 prob-0.5)))
