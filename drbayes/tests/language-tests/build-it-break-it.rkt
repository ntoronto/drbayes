#lang typed/racket

(require plot/typed
         math/distributions
         math/statistics
         math/flonum
         drbayes
         "../test-utils.rkt"
         "gamma-beta.rkt")

(printf "starting...~n")

(interval-max-splits 4)
(define n 5000)

;; Priors
(define/drbayes (standard-program) (beta 30 2))
(define/drbayes (submitted-program) (random))
(define/drbayes (submitted-test) (boolean (const 0.9)))

(define/drbayes (test-result T P)
  (if T
      (< (random) P)
      (> (random) P)))

(define/drbayes (model)
  (let ([P0  (standard-program)]
        [P1  (submitted-program)]
        [P2  (submitted-program)]
        [T0  (submitted-test)]
        [T1  (submitted-test)])
    (let ([Ps  (list P0 P1 P2)]
          [Ts  (list T0 T1)]
          [Rs  (list (list (test-result #t P0)
                           (test-result #t P0)
                           (test-result T0 P0)
                           (test-result T1 P0))
                     (list (test-result #t P1)
                           (test-result #t P1)
                           (test-result T0 P1)
                           (test-result T1 P1))
                     (list (test-result #t P2)
                           (test-result #t P2)
                           (test-result T0 P2)
                           (test-result T1 P2)))])
      (strict-if
       (equal? (list (list #t #t #t #f)
                     (list #t #t #t #t)
                     (list #t #t #t #t))
               Rs)
       (list Ps Ts Rs)
       (fail)))))

(define-type Test-Sample
  (List (Listof Real)
        (Listof Boolean)
        (Listof (Listof Boolean))))

(define-values (ss ws)
  (let-values ([(ss ws)  (drbayes-sample (drbayes (model)) n)])
    (values (cast ss (Listof Test-Sample))
            ws)))

(: program-dist-summary (-> String (Listof Real) (Listof Flonum) Void))
(define (program-dist-summary name Ps ws)
  (printf "Posterior distribution of ~a (mean ~a, stddev ~a):~n" name (mean Ps ws) (stddev Ps ws))
  (displayln (plot (density Ps 1 ws)
                   #:x-min 0 #:x-max 1)))

(define Ps (map (λ ([s : Test-Sample]) (first s)) ss))
(define P0s (map (λ ([P : (Listof Real)]) (first P)) Ps))
(define P1s (map (λ ([P : (Listof Real)]) (second P)) Ps))
(define P2s (map (λ ([P : (Listof Real)]) (third P)) Ps))

(program-dist-summary "P0" P0s ws)
(program-dist-summary "P1" P1s ws)
(program-dist-summary "P2" P2s ws)

(printf "(P0,P1) correlation: ~a~n" (correlation P0s P1s ws))
(printf "Bootstrapped (P0,P1) samples:~n")
(plot (points (map (λ ([s : Test-Sample]) (first s))
                   (sample ((inst discrete-dist Test-Sample) ss ws) (length ss)))
              #:sym 'fullcircle
              #:alpha #i1/32)
      #:x-min 0 #:x-max 1
      #:y-min 0 #:y-max 1)

(: test-dist-summary (-> String (Listof Boolean) (Listof Flonum) Void))
(define (test-dist-summary name Ts ws)
  (define prob-good
    (let-values ([(Ts ws)  (count-samples Ts ws)])
      (pdf (discrete-dist Ts ws) #t)))
  (printf "Probability test ~a is good: ~a~n" name prob-good))

(define Ts (map (λ ([s : Test-Sample]) (second s)) ss))
(define T0s (map (λ ([T : (Listof Boolean)]) (first T)) Ts))
(define T1s (map (λ ([T : (Listof Boolean)]) (second T)) Ts))

(test-dist-summary "T0" T0s ws)
(test-dist-summary "T1" T1s ws)
