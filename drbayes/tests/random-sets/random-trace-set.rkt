#lang typed/racket

(require "../../private/set/indexed.rkt"
         "../../private/set/bool-set.rkt"
         "../../private/set/tree-set.rkt"
         "random-bool-set.rkt")

(provide random-nonempty-trace-set random-trace-set)

(define js
  (list j0
        (left j0) (right j0)
        (left (left j0)) (left (right j0)) (right (left j0)) (right (right j0))))

(: random-nonempty-trace-set (-> Nonempty-Trace-Set))
(define (random-nonempty-trace-set)
  (for/fold ([T : Nonempty-Trace-Set  traces]) ([j  (in-list js)])
    (cond [(< (random) 0.25)
           (let reject ()
             (define I (if (< (random) 0.5) trues falses))
             (let ([T  (trace-set-unproj T j I)])
                   (if (empty-trace-set? T) (reject) T)))]
          [else  T])))

(: random-trace-set (-> Trace-Set))
(define (random-trace-set)
  (cond [(< (random) 0.1)  empty-trace-set]
        [else  (random-nonempty-trace-set)]))
