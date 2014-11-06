#lang typed/racket/base

(require "types.rkt"
         "bottom.rkt"
         "indexed.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(struct: Omega Base-Value ([value : (Indexed-Point Flonum)]) #:transparent)
(define omega? Omega?)

(: omega-ref (Omega Tree-Index -> Flonum))
(define (omega-ref t j) (indexed-point-ref (Omega-value t) j))

(: omega->list (Omega -> (Listof Flonum)))
(define (omega->list t) (indexed-point->list (Omega-value t)))

;; ===================================================================================================

(struct: Trace Base-Value ([value : (Indexed-Point (U Boolean Bottom))]) #:transparent)
(define trace? Trace?)

(: trace-ref (Trace Tree-Index -> (U Boolean Bottom)))
(define (trace-ref t j) (indexed-point-ref (Trace-value t) j))
