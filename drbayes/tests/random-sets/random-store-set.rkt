#lang typed/racket

(require math/distributions
         drbayes/private/set
         "random-prob-set.rkt")

(provide random-nonempty-store-set random-store-set random-store)

(define js
  (list j0
        (left j0) (right j0)
        (left (left j0)) (left (right j0)) (right (left j0)) (right (right j0))))

(: random-store (-> Store-Set Store))
(define (random-store S)
  (cond [(empty-store-set? S)  (raise-argument-error 'random-store "Nonempty-Store-Set" S)]
        [else
         (let loop ([S S] [j j0])
           (define-values (X B L R) (store-set-projs S))
           (Store (delay (random-prob X))
                  (delay (if (bools? B) (make-bottom-trace-value j) (trues? B)))
                  (delay (loop L (left j)))
                  (delay (loop R (right j)))))]))

(: random-nonempty-store-set (-> Nonempty-Store-Set))
(define (random-nonempty-store-set)
  (for/fold ([S : Nonempty-Store-Set  stores]) ([j  (in-list js)])
    (define r (random))
    (cond [(< r #i1/3)
           (let reject ()
             (define B (if (< (random) 0.5) trues falses))
             (let ([S  (store-set-branch-unproj S j B)])
               (if (empty-store-set? S) (reject) S)))]
          [(< r #i2/3)
           (let reject ()
             (define I (random-prob-set))
             (if (or (empty-real-set? I) (reals? I))
                 (reject)
                 (let ([S  (store-set-random-unproj S j I)])
                   (if (empty-store-set? S) (reject) S))))]
          [else  S])))

(: random-store-set (-> Store-Set))
(define (random-store-set)
  (cond [(< (random) 0.1)  empty-store-set]
        [else  (random-nonempty-store-set)]))
