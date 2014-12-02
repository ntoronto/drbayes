#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "../set.rkt"
         "../flonum.rkt"
         "../arrow.rkt"
         "types.rkt"
         "refinement-search.rkt"
         "utils.rkt")

(provide (all-defined-out))

(: refinement-sample-point (-> Refiner Nonempty-Store-Set Indexes (U #f store-sample)))
(define (refinement-sample-point refine S idxs)
  (match (refinement-sample refine S prob-1 prob-1 idxs)
    [(store-set-sample S numer denom)
     (store-sample (store-set-realize S) numer denom)]
    [_  #f]))

;; ===================================================================================================

(: refinement-sample (-> Refiner Store-Set Prob Prob Indexes (U #f store-set-sample)))
(define (refinement-sample refine S numer denom idxs)
  (cond [(empty-store-set? S)  (printf "refinement-sample: empty store set~n")
                               #f]
        [(empty? idxs)  (store-set-sample S numer denom)]
        [else  (let ([idx  (first idxs)]
                     [idxs  (rest idxs)])
                 (if (ifte*-index? idx)
                     (refinement-sample/if refine S numer denom idx idxs)
                     (refinement-sample/ivl refine S numer denom idx idxs)))]))

(: refinement-sample/if (-> Refiner Nonempty-Store-Set Prob Prob ifte*-index Indexes
                            (U #f store-set-sample)))
(define (refinement-sample/if refine S numer denom idx idxs)
  (match-define (ifte*-index j t-idxs f-idxs) idx)
  (define B (store-set-branch-proj S j))
  (cond [(eq? B trues)   (refinement-sample refine S numer denom (append (force t-idxs) idxs))]
        [(eq? B falses)  (refinement-sample refine S numer denom (append (force f-idxs) idxs))]
        [else  (define-values (B new-idxs q)
                 (cond [(< (random) 0.5)  (values trues  (force t-idxs) prob-0.5)]
                       [else              (values falses (force f-idxs) prob-0.5)]))
               (let ([S  (refine (store-set-branch-unproj S j B))])
                 (refinement-sample refine S (prob* numer q) denom (append new-idxs idxs)))]))

(: refinement-sample/ivl (-> Refiner Nonempty-Store-Set Prob Prob random-index Indexes
                             (U #f store-set-sample)))
(define (refinement-sample/ivl refine S numer denom idx idxs)
  (match-define (random-index j split) idx)
  (define I (store-set-random-proj S j))
  (define x (prob-set-sample-point I))
  (cond [(prob? x)
         (define J (Plain-Prob-Interval x x #t #t))
         (define q (prob-set-measure I))
         (let ([S  (refine (store-set-random-unproj S j J))])
           (refinement-sample refine S numer (prob* denom q) idxs))]
        [else
         (printf "I = ~v~n" I)
         #f]))
