#lang typed/racket/base

(require racket/list
         math/flonum
         math/base
         typed/rackunit
         drbayes/private/flonum/flops)

;; Expected number of unique uniform samples, when sampling n outcomes from a set of n
(define mean-prob (- 1.0 (exp -1.0)))

(define -max (flonum->ordinal (- (flexpt 2.0 46.0))))
(define +max (flonum->ordinal (flexpt 2.0 46.0)))

(define (random-flonum)
  (define r (random))
  (cond [(< r #i1/3)  (* 4.0 (- (random) 0.5))]
        [(< r #i2/3)  (* 1420.0 (- (random) 0.5))]
        [else  (ordinal->flonum (random-integer -max +max))]))

(printf "-----------------------------------------------------------------------------------------~n")
(printf "--------- Checking fllog-random coverage on [-2^46,2^46] using local uniformity ---------~n")
(printf "-----------------------------------------------------------------------------------------~n")
(for ([i  (in-range 1 (+ 1 100000))])
  (define a (random-flonum))
  (define a0 (flstep a -50))
  (define a1 (flstep a 50))
  (define n 101)  ; equiv. (+ 1 (flonums-between a0 a1))
  (define m (length (remove-duplicates (build-list n (λ (_) (fllog-random a0 a1))))))
  (define avg (fl (/ m n)))
  (define near-mean? (< (- mean-prob 0.16) avg (+ mean-prob 0.16)))
  (unless near-mean?
    (check-true near-mean? (format "bad: ~v ~v~n" a avg)))
  (when (zero? (modulo i 10000))
    (printf "i = ~v~n~n" i)))
