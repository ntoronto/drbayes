#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base)
         racket/match
         racket/promise
         "types.rkt"
         "real-set.rkt"
         "bool-set.rkt"
         "null-set.rkt"
         "extremal-set.rkt"
         "tree-value.rkt"
         "tree-set.rkt"
         "union.rkt")

(struct: Bottom ([message : (Promise String)]) #:transparent)

(define-syntax bottom (make-rename-transformer #'Bottom))
(define-syntax bottom? (make-rename-transformer #'Bottom?))
(define-syntax bottom-message (make-rename-transformer #'Bottom-message))

(define-type Value (Rec Value (U Flonum Boolean Null (Pair Value Value) Omega Trace tagged-value)))
(define-type Maybe-Value (U Value Bottom))

(struct: tagged-value Base-Value ([tag : Symbol] [value : Value]) #:transparent)

(: value? (Value -> Boolean))
(define (value? v)
  (cond [(flonum? v)        (< -inf.0 v +inf.0)]
        [(boolean? v)       #t]
        [(null? v)          #t]
        [(pair? v)          (and (value? (car v)) (value? (cdr v)))]
        [(omega? v)         #t]
        [(trace? v)         #t]
        [(tagged-value? v)  (value? (tagged-value-value v))]))

;; ===================================================================================================
;; Tagged values

(: value-tag (Value -> Tag))
(define (value-tag v)
  (cond [(flonum? v)        real-tag]
        [(boolean? v)       bool-tag]
        [(null? v)          null-tag]
        [(pair? v)          pair-tag]
        [(omega? v)         omega-tag]
        [(trace? v)         trace-tag]
        [(tagged-value? v)  (tagged-value-tag v)]))

;; ===================================================================================================
;; Ref

(: value-pair-ref (Value Pair-Index -> Maybe-Value))
(define (value-pair-ref v j)
  (cond [(pair? v)  (pair-ref v j)]
        [else  (bottom (delay (format "value-pair-ref: expected Pair; given ~a" v)))]))

(: pair-ref ((Pair Value Value) Pair-Index -> Maybe-Value))
(define (pair-ref x j)
  (match-define (cons x1 x2) x)
  (cond [(eq? j 'fst)  x1]
        [(eq? j 'snd)  x2]
        [(zero? j)     x1]
        [else  (value-pair-ref x2 (- j 1))]))

;; ===================================================================================================
;; Singleton

(: value->singleton (Value -> Bot-Entry))
(define (value->singleton v)
  (cond [(flonum? v)   (flonum->singleton v)]
        [(boolean? v)  (boolean->singleton v)]
        [(null? v)     nulls]
        [(pair? v)     (pair->singleton v)]
        [(tagged-value? v)  (tagged-value->singleton v)]
        [else  (raise-argument-error 'value->singleton
                                     "Flonum, Boolean, Null, Pair or tagged-value" v)]))

(: pair->singleton ((Pair Value Value) -> Bot-Basic))
(define (pair->singleton x)
  (Nonextremal-Pair-Set (value->singleton (car x))
                        (value->singleton (cdr x))))

(: tagged-value->singleton (tagged-value -> Bot-Tagged))
(define (tagged-value->singleton v)
  (match-define (tagged-value tag val) v)
  (bot-tagged tag (value->singleton val)))
