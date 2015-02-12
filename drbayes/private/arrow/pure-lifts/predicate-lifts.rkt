#lang typed/racket/base

(require racket/promise
         racket/flonum
         "../../set.rkt"
         "../types.rkt"
         "make-predicate-lift.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Type predicates

(define-values (real?/bot real?/pre) (predicate/prim 'real? flonum? reals not-reals))
(define-values (null?/bot null?/pre) (predicate/prim 'null? null? nulls not-nulls))
(define-values (pair?/bot pair?/pre) (predicate/prim 'pair? pair? pairs not-pairs))
(define-values (boolean?/bot boolean?/pre) (predicate/prim 'boolean? boolean? bools not-bools))

;; ===================================================================================================
;; Real predicates

(: real-predicate/prim (-> Symbol (-> Flonum Boolean) Nonempty-Real-Set Nonempty-Real-Set
                           (Values (-> Bot-Arrow) (-> Pre-Arrow))))
(define (real-predicate/prim name p? At Af)
  (predicate/prim
   name
   (λ (a) (if (flonum? a) (p? a) (bottom (delay (format "~a: expected Flonum; given ~e" a)))))
   (bot-basic At)
   (bot-basic Af)))

(define nonzero-reals
  (real-set-union (plain-real-interval -inf.0 0.0 #f #f)
                  (plain-real-interval 0.0 +inf.0 #f #f)))

(define-values (zero?/bot zero?/pre)
  (real-predicate/prim 'zero?
                       (λ: ([x : Flonum]) (fl= x 0.0))
                       zero-interval
                       nonzero-reals))

(define-values (negative?/bot negative?/pre)
  (real-predicate/prim 'negative?
                       (λ: ([x : Flonum]) (x . fl< . 0.0))
                       negative-interval
                       nonnegative-interval))

(define-values (positive?/bot positive?/pre)
  (real-predicate/prim 'positive?
                       (λ: ([x : Flonum]) (x . fl> . 0.0))
                       positive-interval
                       nonpositive-interval))

(define-values (nonpositive?/bot nonpositive?/pre)
  (real-predicate/prim 'nonpositive?
                       (λ: ([x : Flonum]) (x . fl<= . 0.0))
                       nonpositive-interval
                       positive-interval))

(define-values (nonnegative?/bot nonnegative?/pre)
  (real-predicate/prim 'nonnegative?
                       (λ: ([x : Flonum]) (x . fl>= . 0.0))
                       nonnegative-interval
                       negative-interval))
