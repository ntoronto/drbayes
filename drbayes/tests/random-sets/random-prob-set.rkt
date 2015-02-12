#lang typed/racket

(require math/distributions
         math/flonum
         drbayes/private/set
         drbayes/private/flonum
         "../test-utils.rkt")

(provide (all-defined-out))

(define prob-endpoint-dist
  (discrete-dist (build-list 11 (λ ([i : Index]) (assert (flonum->prob (/ (fl i) 10.0)) prob?)))))

(define random-probs
  (build-list 21 (λ ([i : Index]) (assert (flonum->prob (/ (fl i) 20.0)) prob?))))

(: random-prob-interval (-> (Discrete-Dist Prob) Prob-Interval))
(define (random-prob-interval dist)
  (define a (sample dist))
  (define b (sample dist))
  (define a? (< (random) 0.5))
  (define b? (< (random) 0.5))
  (prob-interval (prob-min a b) (prob-max a b) a? b?))

(: random-prob (-> Prob-Set Prob))
(define (random-prob I)
  (if (empty-prob-set? I)
      (raise-argument-error 'random-prob "Nonempty-Prob-Set" I)
      (random-element (filter (λ ([x : Prob]) (prob-set-member? I x)) random-probs))))

(: random-prob-set (->* [] [(Discrete-Dist Prob)] Prob-Set))
(define (random-prob-set [dist prob-endpoint-dist])
  (define I (random-prob-interval dist))
  (cond [(< (random) 0.5)  I]
        [else  (let-values ([(I _)  (prob-set-join I (random-prob-set dist))])
                 I)]))
