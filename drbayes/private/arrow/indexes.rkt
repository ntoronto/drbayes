#lang typed/racket/base

(require racket/list
         math/flonum
         "../set.rkt"
         "../flonum.rkt"
         "types.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(: make-constant-splitter (-> (Listof+2 Plain-Prob-Interval) Interval-Splitter))
(define ((make-constant-splitter Is) A)
  (reverse
   (for/fold ([Is : (Listof Plain-Prob-Interval)  empty]) ([I  (in-list Is)])
     (let ([I  (prob-interval-intersect I A)])
       (if (empty-prob-set? I) Is (cons I Is))))))
