#lang typed/racket/base

(provide (all-defined-out))

(: random-element (All (A) ((Listof A) -> A)))
(define (random-element xs)
  (list-ref xs (random (length xs))))
