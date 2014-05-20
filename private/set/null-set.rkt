#lang typed/racket/base

(require "types.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(struct: Base-Null-Set Base-Bot-Basic () #:transparent)
(define null-set? Base-Null-Set?)

(define-singleton-type Empty-Null-Set Base-Null-Set empty-null-set)
(define-singleton-type Full-Null-Set Base-Null-Set nulls)
(define-type Null-Set (U Empty-Null-Set Full-Null-Set))

(: null-set-member? (Null-Set Null -> Boolean))
(define (null-set-member? A x)
  (nulls? A))

(: null-set-complement (case-> (Empty-Null-Set -> Full-Null-Set)
                               (Full-Null-Set -> Empty-Null-Set)
                               (Null-Set -> Null-Set)))
(define (null-set-complement A)
  (if (nulls? A) empty-null-set nulls))

(: null-set-subtract (case-> (Full-Null-Set Empty-Null-Set -> Full-Null-Set)
                             (Null-Set Full-Null-Set -> Empty-Null-Set)
                             (Null-Set Null-Set -> Null-Set)))
(define (null-set-subtract A B)
  (if (empty-null-set? B) A empty-null-set))

(: null-set-union (case-> (Null-Set Full-Null-Set -> Full-Null-Set)
                          (Full-Null-Set Null-Set -> Full-Null-Set)
                          (Null-Set Null-Set -> Null-Set)))
(define (null-set-union A B)
  (if (empty-null-set? A) B A))

(: null-set-intersect (case-> (Null-Set Empty-Null-Set -> Empty-Null-Set)
                              (Empty-Null-Set Null-Set -> Empty-Null-Set)
                              (Null-Set Null-Set -> Null-Set)))
(define (null-set-intersect A B)
  (if (nulls? A) B A))

(: null-set-subseteq? (Null-Set Null-Set -> Boolean))
(define (null-set-subseteq? A B)
  (if (empty-null-set? A) #t (nulls? B)))
