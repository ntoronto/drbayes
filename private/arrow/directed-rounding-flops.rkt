#lang typed/racket/base

(require racket/fixnum
         math/flonum
         (only-in racket/math pi))

(provide (all-defined-out))

;; Floating-point pi is a little smaller than actual pi, so we bump it a bit to use it as a bound
(define -pi/2/rndd (flprev* (/ pi -2.0)))
(define pi/2/rndu (flnext* (/ pi 2.0)))
(define -pi/rndd (flprev* (- pi)))
(define pi/rndu (flnext* pi))

(: flstep* (Flonum Fixnum -> Flonum))
(define (flstep* x n)
  (cond [(n . fx> . 0)
         (let loop ([x  (flnext* x)] [n  (- n 1)])
           (cond [(fx= n 0)  x]
                 [else  (loop (flnext* x) (- n 1))]))]
        [(n . fx< . 0)
         (let loop ([x  (flprev* x)] [n  (+ n 1)])
           (cond [(fx= n 0)  x]
                 [else  (loop (flprev* x) (+ n 1))]))]
        [else  x]))

(: fl2->fl/rndd (Flonum Flonum Flonum -> Flonum))
(define (fl2->fl/rndd x0 x1 mn)
  (if (x1 . fl< . 0.0) (flmax mn (flprev* x0)) x0))

(: fl2->fl/rndu (Flonum Flonum Flonum -> Flonum))
(define (fl2->fl/rndu x0 x1 mx)
  (if (x1 . fl> . 0.0) (flmin mx (flnext* x0)) x0))

(define-syntax-rule (make-unary-flops/rnd flop/error mn mx)
  (values
   (λ: ([x : Flonum])
     (let-values ([(y0 y1)  (flop/error x)])
       (fl2->fl/rndd y0 y1 mn)))
   (λ: ([x : Flonum])
     (let-values ([(y0 y1)  (flop/error x)])
       (fl2->fl/rndu y0 y1 mx)))))

(define-syntax-rule (make-binary-flops/rnd flop/error mn mx)
  (values
   (λ: ([x : Flonum] [y : Flonum])
     (let-values ([(z0 z1)  (flop/error x y)])
       (fl2->fl/rndd z0 z1 mn)))
   (λ: ([x : Flonum] [y : Flonum])
     (let-values ([(z0 z1)  (flop/error x y)])
       (fl2->fl/rndu z0 z1 mx)))))

(define-syntax-rule (make-unary-flops/fake-rnd flop steps-down steps-up mn mx)
  (values (λ: ([x : Flonum]) (flmax mn (flstep* (flop x) (- steps-down))))
          (λ: ([x : Flonum]) (flmin mx (flstep* (flop x) steps-up)))))

(define-syntax-rule (make-binary-flops/fake-rnd flop steps-down steps-up mn mx)
  (values (λ: ([x : Flonum] [y : Flonum]) (flmax mn (flstep* (flop x y) (- steps-down))))
          (λ: ([x : Flonum] [y : Flonum]) (flmin mx (flstep* (flop x y) steps-up)))))

(define-syntax-rule (fllog/error x) (fl2log x 0.0))
(define-syntax-rule (flrecip/error x) (fl//error 1.0 x))

(define-values (flsqr/rndd flsqr/rndu) (make-unary-flops/rnd flsqr/error 0.0 +inf.0))
(define-values (flsqrt/rndd flsqrt/rndu) (make-unary-flops/rnd flsqrt/error 0.0 +inf.0))
(define-values (flexp/rndd flexp/rndu) (make-unary-flops/rnd flexp/error 0.0 +inf.0))
(define-values (fllog/rndd fllog/rndu) (make-unary-flops/rnd fllog/error -inf.0 +inf.0))
(define-values (flrecip/rndd flrecip/rndu) (make-unary-flops/rnd flrecip/error -inf.0 +inf.0))

(define-values (fl+/rndd fl+/rndu) (make-binary-flops/rnd fl+/error -inf.0 +inf.0))
(define-values (fl-/rndd fl-/rndu) (make-binary-flops/rnd fl-/error -inf.0 +inf.0))
(define-values (fl*/rndd fl*/rndu) (make-binary-flops/rnd fl*/error -inf.0 +inf.0))
(define-values (fl//rndd fl//rndu) (make-binary-flops/rnd fl//error -inf.0 +inf.0))

(define-values (flsin/rndd flsin/rndu) (make-unary-flops/fake-rnd flsin 1 1 -1.0 1.0))
(define-values (flcos/rndd flcos/rndu) (make-unary-flops/fake-rnd flcos 1 1 -1.0 1.0))
(define-values (fltan/rndd fltan/rndu) (make-unary-flops/fake-rnd fltan 1 1 -inf.0 +inf.0))
(define-values (flasin/rndd flasin/rndu) (make-unary-flops/fake-rnd flasin 1 1 -pi/2/rndd pi/2/rndu))
(define-values (flacos/rndd flacos/rndu) (make-unary-flops/fake-rnd flacos 1 1 0.0 pi/rndu))
(define-values (flatan/rndd flatan/rndu) (make-unary-flops/fake-rnd flatan 1 1 -pi/2/rndd pi/2/rndu))
