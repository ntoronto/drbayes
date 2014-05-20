#lang typed/racket

(require "untyped-utils.rkt")

(provide (all-defined-out))

;; Maybe type

(define-singleton-type Bottom ⊥)
(struct: (X) just ([value : X]) #:transparent)
(define-type (Maybe X) (U Bottom (just X)))

(: unjust (All (X) ((U Bottom (just X)) -> (U Bottom X))))
(define (unjust v)
  (if (⊥? v) v (just-value v)))
