#lang typed/racket/base

(require "types.rkt"
         "bottom.rkt"
         "indexed.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(struct: Omega Base-Value ([value : (Indexed-Point Flonum)]) #:transparent)
(define omega? Omega?)

(: omega-value (Omega -> Flonum))
(: omega-left (Omega -> Omega))
(: omega-right (Omega -> Omega))
(: omega-ref (Omega Tree-Index -> Flonum))
(: omega->list (Omega -> (Listof Flonum)))

(define (omega-value t) (indexed-point-value (Omega-value t)))
(define (omega-left t) (Omega (indexed-point-left (Omega-value t))))
(define (omega-right t) (Omega (indexed-point-right (Omega-value t))))
(define (omega-ref t j) (indexed-point-ref (Omega-value t) j))
(define (omega->list t) (indexed-point->list (Omega-value t)))

;; ===================================================================================================

(struct: Trace Base-Value ([value : (Indexed-Point (U Boolean Bottom))]) #:transparent)
(define trace? Trace?)

(: trace-value (Trace -> (U Boolean Bottom)))
(: trace-left (Trace -> Trace))
(: trace-right (Trace -> Trace))
(: trace-ref (Trace Tree-Index -> (U Boolean Bottom)))
(: trace->list (Trace -> (Listof (U Boolean Bottom))))

(define (trace-value t) (indexed-point-value (Trace-value t)))
(define (trace-left t) (Trace (indexed-point-left (Trace-value t))))
(define (trace-right t) (Trace (indexed-point-right (Trace-value t))))
(define (trace-ref t j) (indexed-point-ref (Trace-value t) j))
(define (trace->list t) (indexed-point->list (Trace-value t)))
