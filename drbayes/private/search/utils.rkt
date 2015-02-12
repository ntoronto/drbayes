#lang typed/racket/base

(require "../flonum.rkt")

(provide (all-defined-out))

(define prob-0.5 (assert (flonum->prob 0.5) prob?))
(define prob-0.5/rndd (assert (flonum->prob/rndd 0.5) prob?))
(define prob-0.5/rndu (assert (flonum->prob/rndu 0.5) prob?))

(: make-prob-normalize-first (-> (-> Prob Prob (U Prob Bad-Prob))
                                 (-> Prob Prob (U Prob Bad-Prob))
                                 (-> Prob Prob)
                                 (-> Prob Prob)
                                 (-> Prob Prob Prob)))
;; Returns p1/(p1+p2)
(define ((make-prob-normalize-first prob+ prob/ prob-reduce1 prob-reduce2) p1 p2)
  (cond [(prob-0? p1)  prob-0]
        [(prob-0? p2)  prob-1]
        [else
         (let loop ([p1 p1] [p2 p2])
           (define p (prob+ p1 p2))
           (cond [(prob? p)
                  (define q (prob/ p1 p))
                  (if (prob? q) q prob-1)]
                 [else
                  (loop (prob-reduce1 p1)
                        (prob-reduce2 p2))]))]))

(define prob-normalize-first
  (make-prob-normalize-first
   prob+
   prob/
   (λ (p) (prob* p prob-0.5))
   (λ (p) (prob* p prob-0.5))))

(define prob-normalize-first/rndd
  (make-prob-normalize-first
   prob+/rndu
   prob//rndd
   (λ (p) (prob*/rndd p prob-0.5/rndd))
   (λ (p) (prob*/rndu p prob-0.5/rndu))))

(define prob-normalize-first/rndu
  (make-prob-normalize-first
   prob+/rndd
   prob//rndu
   (λ (p) (prob*/rndu p prob-0.5/rndu))
   (λ (p) (prob*/rndd p prob-0.5/rndd))))

(: prob-normalize-first/ivl (-> Prob Prob Prob Prob (Values Prob Prob)))
(define (prob-normalize-first/ivl x0 x1 y0 y1)
  (define z00/rndd (prob-normalize-first/rndd x0 y0))
  (define z01/rndd (prob-normalize-first/rndd x0 y1))
  (define z10/rndd (prob-normalize-first/rndd x1 y0))
  (define z11/rndd (prob-normalize-first/rndd x1 y1))
  (define z00/rndu (prob-normalize-first/rndu x0 y0))
  (define z01/rndu (prob-normalize-first/rndu x0 y1))
  (define z10/rndu (prob-normalize-first/rndu x1 y0))
  (define z11/rndu (prob-normalize-first/rndu x1 y1))
  (values (prob-min (prob-min z00/rndd z01/rndd)
                    (prob-min z10/rndd z11/rndd))
          (prob-max (prob-max z00/rndu z01/rndu)
                    (prob-max z10/rndu z11/rndu))))
