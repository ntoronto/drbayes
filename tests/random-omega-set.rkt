#lang typed/racket

(require math/distributions
         "../private/set/real-set.rkt"
         "../private/set/tree-set.rkt"
         "random-real-set.rkt")

(provide random-nonempty-omega-set random-omega-set)

(define unit-endpoint-dist (discrete-dist '(0.0 0.1 0.2 0.3 0.5 0.7 0.8 0.9 1.0)))

(: random-nonempty-omega-set (-> Nonempty-Omega-Set))
(define (random-nonempty-omega-set)
  (let loop ([n 4])
    (define r (random))
    (cond [(or (r . < . 0.4) (zero? n))  omegas]
          [else
           (let reject ()
             (define I (random-real-set unit-endpoint-dist))
             (cond [(or (empty-real-set? I) (reals? I))  (reject)]
                   [else
                    (omega-set I (omega-children-set (loop (- n 1)) (loop (- n 1))))]))])))

(: random-omega-set (-> Omega-Set))
(define (random-omega-set)
  (cond [((random) . < . 0.1)  empty-omega-set]
        [else  (random-nonempty-omega-set)]))
