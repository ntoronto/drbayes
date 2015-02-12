#lang typed/racket/base

(require "../set.rkt"
         "types.rkt"
         "preimage-mapping.rkt"
         "pure-arrows.rkt")

(provide (all-defined-out))

(define-type Refiner (-> Store-Set (Values Store-Set Boolean)))

(: make-preimage-refiner (-> Pre-Arrow Nonempty-Set Refiner))
(define ((make-preimage-refiner h B) S)
  (cond [(empty-store-set? S)  (values empty-store-set #t)]
        [else
         (define-values (SN exact?) (preimage/pre (run/pre h (set-pair S nulls)) B))
         (let-values ([(S N)  (set-projs SN)])
           (cond [(not (or (empty-set? N) (nulls? N)))
                  (raise-result-error 'preimage-refiner "(U Empty-Set Full-Null-Set)" N)]
                 [else
                  (values (set-take-stores S) exact?)]))]))
