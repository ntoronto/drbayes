#lang typed/racket/base

(require racket/promise
         racket/flonum
         racket/match
         "../set.rkt"
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

(define fail/proc (lower/proc fail/bot))
(define id/proc (lower/proc id/bot))
(define const/proc (λ: ([b : Value]) (lower/proc (const/bot b))))
(define restrict/proc (λ: ([X : Nonempty-Set]) (lower/proc (restrict/bot X))))
(define ref/proc (λ: ([j : Pair-Index]) (lower/proc (ref/bot j))))

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

(: random/proc Proc-Arrow)
(define (random/proc a) (random))

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
  ((expr . &&&/proc . id/proc) . >>>/proc . body))

;; ===================================================================================================
;; Other lifts

(define tag?/proc (λ: ([tag : Tag]) (lower/proc (tag?/bot tag))))
(define tag/proc (λ: ([tag : Tag]) (lower/proc (tag/bot tag))))
(define untag/proc (λ: ([tag : Tag]) (lower/proc (untag/bot tag))))

(define real?/proc real?)
(define null?/proc null?)
(define pair?/proc pair?)
(define boolean?/proc boolean?)

(define scale/proc (λ: ([y : Flonum]) (lower/proc (scale/bot y))))
(define translate/proc (λ: ([y : Flonum]) (lower/proc (translate/bot y))))
(define neg/proc (lower/proc neg/bot))
(define exp/proc (lower/proc exp/bot))
(define log/proc (lower/proc log/bot))
(define sqrt/proc (lower/proc sqrt/bot))
(define asin/proc (lower/proc asin/bot))
(define acos/proc (lower/proc acos/bot))
(define floor/proc (lower/proc floor/bot))
(define ceiling/proc (lower/proc ceiling/bot))
(define round/proc (lower/proc round/bot))
(define truncate/proc (lower/proc truncate/bot))

(define cauchy/proc (lower/proc cauchy/bot))
(define normal/proc (lower/proc normal/bot))

(define +/proc (lower/proc +/bot))
(define -/proc (lower/proc -/bot))

(define negative?/proc (lower/proc negative?/bot))
(define positive?/proc (lower/proc positive?/bot))
(define nonpositive?/proc (lower/proc nonpositive?/bot))
(define nonnegative?/proc (lower/proc nonnegative?/bot))

(define </proc (lower/proc </bot))
(define >/proc (lower/proc >/bot))
(define <=/proc (lower/proc <=/bot))
(define >=/proc (lower/proc >=/bot))

(define abs/proc (lower/proc abs/bot))
(define sqr/proc (lower/proc sqr/bot))
(define recip/proc (lower/proc recip/bot))

(define */proc (lower/proc */bot))
(define //proc (lower/proc //bot))

(define partial-cos/proc (lower/proc partial-cos/bot))
(define partial-sin/proc (lower/proc partial-sin/bot))
