#lang typed/racket/base

(require "../private/set/bool-set.rkt")

(provide (all-defined-out))

(: random-bool-set (-> Bool-Set))
(define (random-bool-set)
  (booleans->bool-set ((random) . < . 0.5) ((random) . < . 0.5)))

(: random-bool (Bool-Set -> Boolean))
(define (random-bool A)
  (cond [(empty-bool-set? A)  (raise-argument-error 'random-bool "Nonempty-Bool-Set" A)]
        [(bools? A)  ((random) . < . 0.5)]
        [else  (trues? A)]))
