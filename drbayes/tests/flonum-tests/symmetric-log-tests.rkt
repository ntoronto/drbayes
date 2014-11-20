#lang typed/racket

(require math/bigfloat
         math/flonum
         math/base
         drbayes/private/flonum
         (only-in drbayes/private/bigfloat bfprob* bfprob/ bfprob+ bfprob- bfprob-midpoint))

(define -inf (flonum->ordinal -inf.0))
(define +inf (flonum->ordinal +inf.0))
(define -log2 (flonum->ordinal (fllog 0.5)))
(define +log2 (flonum->ordinal (fllog 2.0)))

(define (random-flprob)
  (cond [(< (random) 0.05)
         (define r (random))
         (cond [(< r #i1/5)  (flstep -inf.0 (random 10))]
               [(< r #i2/5)  (flstep (fllog 0.5) (- (random 10)))]
               [(< r #i3/5)  (flstep (fllog 2.0) (random 10))]
               [(< r #i4/5)  (flstep +inf.0 (- (random 10)))]
               [else         +nan.0])]
        [(< (random) 0.5)  (flonum->flprob (random))]
        [(< (random) 0.5)  (ordinal->flonum (random-integer -inf (+ 1 -log2)))]
        [else              (ordinal->flonum (random-integer +log2 (+ 1 +inf)))]))

(: make-2d-error-fun (-> (-> Flonum Flonum Flonum)
                         (-> Bigfloat Bigfloat Bigfloat)
                         (-> Flonum Flonum (Values Flonum Flonum Flonum))))
(define ((make-2d-error-fun flop bfop) x y)
  (define z (flop x y))
  (define z* (bigfloat->real (bfop (bf x) (bf y))))
  (define e (min (flulp-error z z*)
                 (flulp-error z (- z*))))
  (values z (fl z*) e))

(: test-2d-fun (-> Any
                   (-> Flonum Flonum Flonum)
                   (-> Bigfloat Bigfloat Bigfloat)
                   Flonum
                   Natural
                   Void))
(define (test-2d-fun name flop bfop thresh n)
  (define msg (format "---------- Testing ~a, error threshold ~a, count ~a ----------" name thresh n))
  (define dashes (make-string (string-length msg) #\-))
  (printf "~n~a~n~a~n~a~n" dashes msg dashes)
  (define flop-error (make-2d-error-fun flop bfop))
  (for ([i  (in-range 1 (add1 n))])
    (define x (random-flprob))
    (define y (random-flprob))
    (define-values (z z* e) (flop-error x y))
    (when (or (flnan? x) (flnan? y))
      (unless (flnan? z)
        (printf "not nan-preserving: ~v ~v ~v~n" x y z)))
    (when (> e thresh)
      (printf "~v: x = ~v  y = ~v~n  z = ~v  z* = ~v~n" e x y z (fl z*)))
    (when (zero? (modulo i 10000))
      (printf "i = ~v~n~n" i))))

(: test-2d-domain-boundary (-> Any
                               (-> Flonum Flonum Flonum)
                               Natural
                               (-> (Values Flonum Flonum))
                               Void))
(define (test-2d-domain-boundary name flop n rand)
  (define msg (format "---------- Testing ~a domain boundary, count ~a ----------" name n))
  (define dashes (make-string (string-length msg) #\-))
  (printf "~n~a~n~a~n~a~n" dashes msg dashes)
  (for ([i  (in-range 1 (add1 n))])
    (define-values (x y) (rand))
    (let ([x  (flstep (flonum->flprob x) (- (random 11) 5))]
          [y  (flstep (flonum->flprob y) (- (random 11) 5))])
      (define z (flop x y))
      (when (and (flprob? x) (flprob? y))
        (unless (flprob? z) (printf "not flprob?: ~v ~v ~v~n" x y z))))
    (when (zero? (modulo i 10000))
      (printf "i = ~v~n~n" i))))

(define n 100000)
(test-2d-fun 'flprob* flprob* bfprob* 0.5 n)
(test-2d-fun 'flprob/ flprob/ bfprob/ 0.5 n)
(test-2d-fun 'flprob+ flprob+ bfprob+ 0.5 n)
(test-2d-fun 'flprob- flprob- bfprob- 0.5 n)
(test-2d-fun 'flprob-midpoint flprob-midpoint bfprob-midpoint 2.0 n)

(define m 100000)

(test-2d-domain-boundary 'flprob* flprob* m
                         (λ () (let ([x  (random)])
                                 (values x (/ 0.5 x)))))

(test-2d-domain-boundary 'flprob/ flprob/ m
                         (λ () (let ([y  (random)])
                                 (values (* 0.5 y) y))))

(test-2d-domain-boundary 'flprob+ flprob+ m
                         (λ () (let ([x  (* 0.5 (random))])
                                 (values x (- 0.5 x)))))

(test-2d-domain-boundary 'flprob- flprob- m
                         (λ () (let ([y  (* 0.5 (random))])
                                 (values (+ 0.5 y) y))))

(test-2d-domain-boundary 'flprob-midpoint flprob-midpoint m
                         (λ () (let ([y  (* 0.5 (random))])
                                 (values (+ 0.5 y) (- 0.5 y)))))
