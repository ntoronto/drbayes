#lang typed/racket

(require typed/rackunit
         "../private/set.rkt"
         "../private/arrow.rkt"
         "../private/arrow/preimage-mapping.rkt")

(check-true
 (set-equal? (range/pre (run/pre +/pre (set-pair (real-set 0.0 1.0) (real-set 0.0 1.0))))
             (real-set 0.0 2.0)))

(check-true
 (set-equal? (ap/pre (run/pre +/pre (set-pair (real-set 0.0 1.0) (real-set 0.0 1.0)))
                     (real-set 0.0 0.5))
             (set-pair (real-set 0.0 0.5) (real-set 0.0 0.5))))
