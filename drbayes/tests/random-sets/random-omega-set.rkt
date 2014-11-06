#lang typed/racket

(require math/distributions
         "../../private/set/indexed.rkt"
         "../../private/set/real-set.rkt"
         "../../private/set/tree-set.rkt"
         "random-real-set.rkt")

(provide random-nonempty-omega-set random-omega-set)

(define unit-endpoint-dist (discrete-dist '(0.0 0.1 0.2 0.3 0.5 0.7 0.8 0.9 1.0)))

(define js
  (list j0
        (left j0) (right j0)
        (left (left j0)) (left (right j0)) (right (left j0)) (right (right j0))))

(: random-nonempty-omega-set (-> Nonempty-Omega-Set))
(define (random-nonempty-omega-set)
  (for/fold ([Ω : Nonempty-Omega-Set  omegas]) ([j  (in-list js)])
    (cond [(< (random) 0.25)
           (let reject ()
             (define I (random-real-set unit-endpoint-dist))
             (if (or (empty-real-set? I) (reals? I))
                 (reject)
                 (let ([Ω  (omega-set-unproj Ω j I)])
                   (if (empty-omega-set? Ω) (reject) Ω))))]
          [else  Ω])))

(: random-omega-set (-> Omega-Set))
(define (random-omega-set)
  (cond [(< (random) 0.1)  empty-omega-set]
        [else  (random-nonempty-omega-set)]))
