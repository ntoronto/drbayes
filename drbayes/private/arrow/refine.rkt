#lang typed/racket/base

(require "../set.rkt"
         "types.rkt"
         "preimage-mapping.rkt"
         "pure-arrows.rkt")

(provide (all-defined-out))

(define-type Refiner (-> Store-Set Store-Set))

(: make-preimage-refiner (-> Pre-Arrow Nonempty-Set Refiner))
(define ((make-preimage-refiner h B) S)
  (cond [(empty-store-set? S)  empty-store-set]
        [else
         (let-values ([(S N)  (set-projs (preimage/pre (run/pre h (set-pair S nulls)) B))])
           (cond [(not (or (empty-set? N) (nulls? N)))
                  (raise-result-error 'preimage-refiner "(U Empty-Set Full-Null-Set)" N)]
                 [else
                  (set-take-stores S)]))]))
