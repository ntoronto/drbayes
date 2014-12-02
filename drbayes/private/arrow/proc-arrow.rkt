#lang typed/racket/base

(require racket/promise
         racket/flonum
         racket/match
         "../set.rkt"
         "../flonum.rkt"
         "types.rkt"
         "pure-arrows.rkt"
         "pure-lifts.rkt")

(provide (all-defined-out))

(define-type Proc-Arrow (Value -> Value))

(: lower/proc (Bot-Arrow -> Proc-Arrow))
(define ((lower/proc f) a)
  (define b (f a))
  (if (bottom? b) (error 'drbayes (force (bottom-message b))) b))

;; ===================================================================================================
;; Basic lifts

(define (fail/proc) (lower/proc (fail/bot)))
(define (id/proc) (lower/proc (id/bot)))
(define const/proc (λ: ([b : Value]) (lower/proc (const/bot b))))
(define restrict/proc (λ: ([X : Nonempty-Set]) (lower/proc (restrict/bot X))))
(define (fst/proc) (lower/proc (fst/bot)))
(define (snd/proc) (lower/proc (snd/bot)))
(define list-ref/proc (λ: ([j : Natural]) (lower/proc (list-ref/bot j))))

;; ===================================================================================================
;; Combinators

(: >>>/proc (Proc-Arrow Proc-Arrow -> Proc-Arrow))
(define ((>>>/proc f1 f2) a)
  (f2 (f1 a)))

(: &&&/proc (Proc-Arrow Proc-Arrow -> Proc-Arrow))
(define ((&&&/proc f1 f2) a)
  (cons (f1 a) (f2 a)))

(: ifte/proc (Proc-Arrow Proc-Arrow Proc-Arrow -> Proc-Arrow))
(define ((ifte/proc f1 f2 f3) a)
  (if (f1 a) (f2 a) (f3 a)))

(: lazy/proc ((Promise Proc-Arrow) -> Proc-Arrow))
(define ((lazy/proc f) a) ((force f) a))

(define ifte*/proc ifte/proc)

(: store-uniform/proc (-> Proc-Arrow))
(define ((store-uniform/proc) a) (prob-random prob-0 prob-1))

(: boolean/proc (Flonum -> Proc-Arrow))
(define ((boolean/proc p) a) ((random) . < . p))

;; ===================================================================================================
;; Derived combinators

(define null/proc (const/proc null))

(: list/proc (Proc-Arrow * -> Proc-Arrow))
(define (list/proc . ks)
  (foldr &&&/proc null/proc ks))

(: apply/proc (Proc-Arrow (Listof Proc-Arrow) -> Proc-Arrow))
(define (apply/proc body args)
  ((list/proc (apply list/proc args)) . >>>/proc . body))

(: let/proc (Proc-Arrow Proc-Arrow -> Proc-Arrow))
(define (let/proc expr body)
  ((expr . &&&/proc . (id/proc)) . >>>/proc . body))

;; ===================================================================================================
;; Other lifts

(define (equal?/proc) (lower/proc (equal?/bot)))

(define tag?/proc (λ: ([tag : Tag]) (lower/proc (tag?/bot tag))))
(define tag/proc (λ: ([tag : Tag]) (lower/proc (tag/bot tag))))
(define untag/proc (λ: ([tag : Tag]) (lower/proc (untag/bot tag))))

(define (real?/proc) (lower/proc (real?/bot)))
(define (null?/proc) (lower/proc (null?/bot)))
(define (pair?/proc) (lower/proc (pair?/bot)))
(define (boolean?/proc) (lower/proc (boolean?/bot)))

(define scale/proc (λ: ([y : Flonum]) (lower/proc (scale/bot y))))
(define translate/proc (λ: ([y : Flonum]) (lower/proc (translate/bot y))))
(define (neg/proc) (lower/proc (neg/bot)))
(define (exp/proc) (lower/proc (exp/bot)))
(define (log/proc) (lower/proc (log/bot)))
(define (expm1/proc) (lower/proc (expm1/bot)))
(define (log1p/proc) (lower/proc (log1p/bot)))
(define (sqrt/proc) (lower/proc (sqrt/bot)))
(define (asin/proc) (lower/proc (asin/bot)))
(define (acos/proc) (lower/proc (acos/bot)))
(define (floor/proc) (lower/proc (floor/bot)))
(define (ceiling/proc) (lower/proc (ceiling/bot)))
(define (round/proc) (lower/proc (round/bot)))
(define (truncate/proc) (lower/proc (truncate/bot)))
(define (abs/proc) (lower/proc (abs/bot)))
(define (sqr/proc) (lower/proc (sqr/bot)))
(define (recip/proc) (lower/proc (recip/bot)))
(define (partial-cos/proc) (lower/proc (partial-cos/bot)))
(define (partial-sin/proc) (lower/proc (partial-sin/bot)))

(define (+/proc) (lower/proc (+/bot)))
(define (-/proc) (lower/proc (-/bot)))
(define (*/proc) (lower/proc (*/bot)))
(define (//proc) (lower/proc (//bot)))

(define (normal-inv-cdf/proc) (lower/proc (normal-inv-cdf/bot)))
(define (cauchy-inv-cdf/proc) (lower/proc (cauchy-inv-cdf/bot)))
(define (uniform-inv-cdf/proc) (lower/proc (uniform-inv-cdf/bot)))

(define (zero?/proc) (lower/proc (zero?/bot)))
(define (negative?/proc) (lower/proc (negative?/bot)))
(define (positive?/proc) (lower/proc (positive?/bot)))
(define (nonpositive?/proc) (lower/proc (nonpositive?/bot)))
(define (nonnegative?/proc) (lower/proc (nonnegative?/bot)))

(define (</proc) (lower/proc (</bot)))
(define (>/proc) (lower/proc (>/bot)))
(define (<=/proc) (lower/proc (<=/bot)))
(define (>=/proc) (lower/proc (>=/bot)))
(define (=/proc) (lower/proc (=/bot)))
