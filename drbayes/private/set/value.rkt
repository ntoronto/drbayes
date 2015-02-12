#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base)
         racket/match
         racket/promise
         "types.rkt"
         "bottom.rkt"
         "real-set.rkt"
         "bool-set.rkt"
         "null-set.rkt"
         "extremal-set.rkt"
         "store.rkt"
         "store-set.rkt"
         "union.rkt"
         "../flonum.rkt"
         )

(define-type Value (Rec Value (U Flonum
                                 Prob
                                 Boolean
                                 Null
                                 (Pair Value Value)
                                 tagged-value
                                 Store)))

(define-type Maybe-Value (U Value Bottom))

(struct: tagged-value Base-Value ([tag : Symbol] [value : Value]) #:transparent)

(: value? (Value -> Boolean))
(define (value? v)
  (cond [(flonum? v)        (< -inf.0 v +inf.0)]
        [(prob? v)          #t]
        [(boolean? v)       #t]
        [(null? v)          #t]
        [(pair? v)          (and (value? (car v)) (value? (cdr v)))]
        [(store? v)         #t]
        [(tagged-value? v)  (value? (tagged-value-value v))]))

;; ===================================================================================================
;; Tagged values

(: value-tag (Value -> Tag))
(define (value-tag v)
  (cond [(flonum? v)        real-tag]
        [(prob? v)          prob-tag]
        [(boolean? v)       bool-tag]
        [(null? v)          null-tag]
        [(pair? v)          pair-tag]
        [(store? v)         store-tag]
        [(tagged-value? v)  (tagged-value-tag v)]))

;; ===================================================================================================
;; Projections

(: value-fst (Value -> Maybe-Value))
(define (value-fst v)
  (cond [(pair? v)  (car v)]
        [else  (bottom (delay (format "value-fst: expected pair; given ~e" v)))]))

(: value-snd (Value -> Maybe-Value))
(define (value-snd v)
  (cond [(pair? v)  (cdr v)]
        [else  (bottom (delay (format "value-snd: expected pair; given ~e" v)))]))

(: value-list-ref (Value Natural -> Maybe-Value))
(define (value-list-ref orig-v j)
  (let loop ([v orig-v] [j j])
    (cond [(null? v)
           (bottom (delay (format "value-list-ref: index out of range; given ~e and ~e" orig-v j)))]
          [(not (pair? v))
           (bottom (delay (format "value-list-ref: expected list; given ~e" orig-v)))]
          [(zero? j)  (car v)]
          [else       (loop (cdr v) (- j 1))])))
