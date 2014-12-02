#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/performance-hint
         math/flonum
         racket/math
         "flops.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Using fl2 functions for directed rounding

(: fl2->fl/rndd (-> Flonum Flonum Flonum Flonum))
(define (fl2->fl/rndd x.hi x.lo mn)
  (if (fl< x.lo 0.0) (flmax mn (flprev* x.hi)) x.hi))

(: fl2->fl/rndu (-> Flonum Flonum Flonum Flonum))
(define (fl2->fl/rndu x.hi x.lo mx)
  (if (fl> x.lo 0.0) (flmin mx (flnext* x.hi)) x.hi))

(define-syntax (define-unary-flops/rnd stx)
  (syntax-case stx ()
    [(_ name flop/error mn mx)
     (with-syntax ([name/rndd  (format-id #'name "~a/rndd" #'name)]
                   [name/rndu  (format-id #'name "~a/rndu" #'name)])
       (syntax/loc stx
         (begin
           (: name/rndd (-> Flonum Flonum))
           (: name/rndu (-> Flonum Flonum))
           (define (name/rndd x) (let-values ([(y.hi y.lo)  (flop/error x)])
                                   (fl2->fl/rndd y.hi y.lo mn)))
           (define (name/rndu x) (let-values ([(y.hi y.lo)  (flop/error x)])
                                   (fl2->fl/rndu y.hi y.lo mx))))))]))

(define-syntax (define-binary-flops/rnd stx)
  (syntax-case stx ()
    [(_ name flop/error mn mx)
     (with-syntax ([name/rndd  (format-id #'name "~a/rndd" #'name)]
                   [name/rndu  (format-id #'name "~a/rndu" #'name)])
       (syntax/loc stx
         (begin
           (: name/rndd (-> Flonum Flonum Flonum))
           (: name/rndu (-> Flonum Flonum Flonum))
           (define (name/rndd x y) (let-values ([(z.hi z.lo)  (flop/error x y)])
                                     (fl2->fl/rndd z.hi z.lo mn)))
           (define (name/rndu x y) (let-values ([(z.hi z.lo)  (flop/error x y)])
                                     (fl2->fl/rndu z.hi z.lo mx))))))]))

;; ===================================================================================================
;; Faking directed rounding using flstep*

(define-syntax (define-unary-flops/fake-rnd stx)
  (syntax-case stx ()
    [(_ name flop steps-down steps-up mn mx)
     (with-syntax ([name/rndd  (format-id #'name "~a/rndd" #'name)]
                   [name/rndu  (format-id #'name "~a/rndu" #'name)])
       (syntax/loc stx
         (begin
           (: name/rndd (-> Flonum Flonum))
           (: name/rndu (-> Flonum Flonum))
           (define (name/rndd x) (flmax mn (flstep* (flop x) (- steps-down))))
           (define (name/rndu x) (flmin mx (flstep* (flop x) steps-up))))))]))

(define-syntax (define-binary-flops/fake-rnd stx)
  (syntax-case stx ()
    [(_ name flop steps-down steps-up mn mx)
     (with-syntax ([name/rndd  (format-id #'name "~a/rndd" #'name)]
                   [name/rndu  (format-id #'name "~a/rndu" #'name)])
       (syntax/loc stx
         (begin
           (: name/rndd (-> Flonum Flonum Flonum))
           (: name/rndu (-> Flonum Flonum Flonum))
           (define (name/rndd x y) (flmax mn (flstep* (flop x y) (- steps-down))))
           (define (name/rndu x y) (flmin mx (flstep* (flop x y) steps-up))))))]))

;; ===================================================================================================
;; Transcendental constants

;; 64-bit floating-point pi is a little smaller than actual pi, so we use it as the lower bound and
;; the next flonum as the upper bound

(define +pi/rndd pi)
(define +pi/rndu (flnext* +pi/rndd))

(define -pi/rndu (- +pi/rndd))
(define -pi/rndd (- +pi/rndu))

;; ===================================================================================================
;; Directed rounding flonum ops

(begin-encourage-inline
  
  (define-unary-flops/rnd flsqr   flsqr/error      0.0 +inf.0)
  (define-unary-flops/rnd flsqrt  flsqrt/error     0.0 +inf.0)
  (define-unary-flops/rnd flexp   flexp/error      0.0 +inf.0)
  (define-unary-flops/rnd fllog   fllog/error   -inf.0 +inf.0)
  (define-unary-flops/rnd flexpm1 flexpm1/error   -1.0 +inf.0)
  (define-unary-flops/rnd fllog1p fllog1p/error -inf.0 +inf.0)
  (define-unary-flops/rnd flrecip flrecip/error -inf.0 +inf.0)
  
  (define-binary-flops/rnd fl+ fl+/error -inf.0 +inf.0)
  (define-binary-flops/rnd fl- fl-/error -inf.0 +inf.0)
  (define-binary-flops/rnd fl* fl*/error -inf.0 +inf.0)
  (define-binary-flops/rnd fl/ fl//error -inf.0 +inf.0)
  
  (define-unary-flops/fake-rnd flsin  flsin  1 1   -1.0   +1.0)
  (define-unary-flops/fake-rnd flcos  flcos  1 1   -1.0   +1.0)
  (define-unary-flops/fake-rnd fltan  fltan  1 1 -inf.0 +inf.0)
  (define-unary-flops/fake-rnd flasin flasin 1 1 (* 0.5 -pi/rndd) (* 0.5 +pi/rndu))
  (define-unary-flops/fake-rnd flacos flacos 1 1       0.0               +pi/rndu)
  (define-unary-flops/fake-rnd flatan flatan 1 1 (* 0.5 -pi/rndd) (* 0.5 +pi/rndu))
    
  (: flneg-sqrt/rndd (-> Flonum Flonum))
  (: flneg-sqrt/rndu (-> Flonum Flonum))
  (define (flneg-sqrt/rndd x) (flneg (flsqrt/rndu x)))
  (define (flneg-sqrt/rndu x) (flneg (flsqrt/rndd x)))
  
  (: flrev-/rndd (-> Flonum Flonum Flonum))
  (: flrev-/rndu (-> Flonum Flonum Flonum))
  (define (flrev-/rndd z x) (fl-/rndd x z))
  (define (flrev-/rndu z x) (fl-/rndu x z))
  
  (: flrev//rndd (-> Flonum Flonum Flonum))
  (: flrev//rndu (-> Flonum Flonum Flonum))
  (define (flrev//rndd c a) (fl//rndd a c))
  (define (flrev//rndu c a) (fl//rndu a c))
  
  )
