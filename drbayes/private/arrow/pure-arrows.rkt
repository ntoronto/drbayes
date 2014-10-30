#lang typed/racket/base

(require racket/promise
         "../set.rkt"
         "preimage-mapping.rkt")

(provide (all-defined-out))

(define-type Bot-Arrow (Value -> Maybe-Value))
(define-type Pre-Arrow (Nonempty-Set -> Pre-Mapping))

(: run/bot (case-> (Bot-Arrow Bottom -> Bottom)
                   (Bot-Arrow Maybe-Value -> Maybe-Value)))
(define (run/bot f a)
  (if (bottom? a) a (f a)))

(: run/pre (case-> (Pre-Arrow Empty-Set -> Empty-Pre-Mapping)
                   (Pre-Arrow Set -> Pre-Mapping)))
(define (run/pre h A)
  (if (empty-set? A) empty-pre-mapping (h A)))

;; ===================================================================================================
;; Basic computable lifts

;; ---------------------------------------------------------------------------------------------------
;; Failure

(: fail/bot Bot-Arrow)
(define (fail/bot a) (bottom (delay "fail")))

(: fail/pre Pre-Arrow)
(define (fail/pre A) empty-pre-mapping)

;; ---------------------------------------------------------------------------------------------------
;; Identity function

(: id/bot Bot-Arrow)
(define (id/bot a) a)

(: id/pre Pre-Arrow)
(define (id/pre A) (nonempty-pre-mapping A (λ (B) B)))

;; ---------------------------------------------------------------------------------------------------
;; Domain restriction

(: restrict/bot (Nonempty-Set -> Bot-Arrow))
(define ((restrict/bot X) a)
  (cond [(set-member? X a)  a]
        [else  (bottom (delay (format "restrict: expected value in ~e; given ~e" X a)))]))

(: restrict/pre (Nonempty-Set -> Pre-Arrow))
(define ((restrict/pre X) A)
  (let ([A  (set-intersect A X)])
    (cond [(empty-set? A)  empty-pre-mapping]
          [else  (nonempty-pre-mapping A (λ (B) B))])))

;; ---------------------------------------------------------------------------------------------------
;; Constant functions

(: const/bot (Value -> Bot-Arrow))
(define ((const/bot b) a) b)

(: const/pre (Value -> Pre-Arrow))
(define (const/pre b)
  (define B (value->singleton b))
  (λ (A) (nonempty-pre-mapping B (λ (B) A))))

;; ---------------------------------------------------------------------------------------------------
;; Pair and list projections

(: ref/bot (Pair-Index -> Bot-Arrow))
(define ((ref/bot n) a) (value-pair-ref a n))

(: ref/pre (Pair-Index -> Pre-Arrow))
(define ((ref/pre n) A) (pre-mapping (set-proj A n) (λ (B) (set-unproj A n B))))

;; ===================================================================================================
;; Arrow combinators (except the uncomputable `arr')

;; ---------------------------------------------------------------------------------------------------
;; Composition

(: >>>/bot (Bot-Arrow Bot-Arrow -> Bot-Arrow))
(define ((>>>/bot f1 f2) a)
  (run/bot f2 (f1 a)))

(: >>>/pre (Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((>>>/pre h1 h2) A)
  (let* ([h1  (h1 A)]
         [h2  (run/pre h2 (range/pre h1))])
    (compose/pre h2 h1)))

;; ---------------------------------------------------------------------------------------------------
;; Pairing

(: &&&/bot (Bot-Arrow Bot-Arrow -> Bot-Arrow))
(define ((&&&/bot f1 f2) a)
  (define b1 (f1 a))
  (cond [(bottom? b1)  b1]
        [else  (define b2 (f2 a))
               (cond [(bottom? b2)  b2]
                     [else  (cons b1 b2)])]))

(: &&&/pre (Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((&&&/pre h1 h2) A)
  (let ([h1  (h1 A)]
        [h2  (h2 A)])
    (pair/pre h1 h2)))

;; ---------------------------------------------------------------------------------------------------
;; Partial if-then-else

(: ifte/bot (Bot-Arrow Bot-Arrow Bot-Arrow -> Bot-Arrow))
(define ((ifte/bot f1 f2 f3) a)
  (define b (f1 a))
  (cond [(bottom? b)  b]
        [(eq? b #t)  (f2 a)]
        [(eq? b #f)  (f3 a)]
        [else  (bottom (delay (format "ifte/bot: expected Boolean condition; given ~e" b)))]))

(: ifte/pre (Pre-Arrow Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((ifte/pre h1 h2 h3) A)
  (let* ([h1  (h1 A)]
         [h2  (run/pre h2 (ap/pre h1 trues))]
         [h3  (run/pre h3 (ap/pre h1 falses))])
    (uplus/pre h2 h3)))

;; ---------------------------------------------------------------------------------------------------
;; Laziness

(: lazy/bot ((Promise Bot-Arrow) -> Bot-Arrow))
(define ((lazy/bot f) a) ((force f) a))

(: lazy/pre ((Promise Pre-Arrow) -> Pre-Arrow))
(define ((lazy/pre h) A) ((force h) A))
