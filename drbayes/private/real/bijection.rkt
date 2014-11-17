#lang typed/racket/base

(require racket/match
         racket/list
         "../set.rkt"
         "../flonum.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Image computation for monotone R -> R functions

(: monotone-apply (-> (-> Nonempty-Interval Real-Set) Set Set))
(define (monotone-apply f A)
  (define B (real-set-map f (set-take-reals A)))
  (if (empty-real-set? B) empty-set B))

(: strictly-monotone-image (-> Boolean (-> Flonum Flonum) (-> Flonum Flonum) Set Set))
(define (strictly-monotone-image inc? f/rndd f/rndu A)
  (monotone-apply (λ (A)
                    (define-values (a1 a2 a1? a2?) (interval-fields A))
                    (cond [inc?  (interval (f/rndd a1) (f/rndu a2) a1? a2?)]
                          [else  (interval (f/rndd a2) (f/rndu a1) a2? a1?)]))
                  A))

;; ===================================================================================================
;; Bijections (invertible functions and their inverses)

(struct: bijection ([inc? : Boolean]
                    [domain : Nonempty-Interval]
                    [range : Nonempty-Interval]
                    [fb/rndd : (-> Flonum Flonum)]
                    [fb/rndu : (-> Flonum Flonum)]
                    [fa/rndd : (-> Flonum Flonum)]
                    [fa/rndu : (-> Flonum Flonum)])
  #:transparent)

(: bijection-inverse (-> bijection bijection))
(define (bijection-inverse f)
  (match-define (bijection inc? X Y fb/rndd fb/rndu fa/rndd fa/rndu) f)
  (bijection inc? Y X fa/rndd fa/rndu fb/rndd fb/rndu))

(: bijection-image (-> bijection (-> Set Set)))
(define (bijection-image f)
  (match-define (bijection inc? X Y fb/rndd fb/rndu _ _) f)
  (λ (A) (set-intersect Y (strictly-monotone-image inc? fb/rndd fb/rndu (set-intersect A X)))))

(: bijection-preimage (-> bijection (-> Set (-> Set Set))))
(define (bijection-preimage f)
  (match-define (bijection inc? X Y _ _ fa/rndd fa/rndu) f)
  (λ (A)
    (let ([A  (set-intersect A X)])
      (λ (B) (set-intersect A (strictly-monotone-image inc? fa/rndd fa/rndu (set-intersect B Y)))))))

;; ===================================================================================================
;; Some bijections

(: zeros+ (-> (-> Flonum Flonum) (-> Flonum Flonum)))
(define ((zeros+ f) x)
  (f (if (zero? x) 0.0 x)))

(: zeros- (-> (-> Flonum Flonum) (-> Flonum Flonum)))
(define ((zeros- f) x)
  (f (if (zero? x) -0.0 x)))

(define bij-neg
  (bijection #f reals reals
             flneg flneg
             flneg flneg))

(define bij-exp
  (bijection #t reals positive-interval
             flexp/rndd flexp/rndu
             fllog/rndd fllog/rndu))

(define bij-log (bijection-inverse bij-exp))

(define bij-expm1
  (bijection #t reals (Nonextremal-Interval -1.0 +inf.0 #f #f)
             flexpm1/rndd flexpm1/rndu
             fllog1p/rndd fllog1p/rndu))

(define bij-log1p (bijection-inverse bij-expm1))

(define bij-sqrt
  (bijection #t nonnegative-interval nonnegative-interval
             flsqrt/rndd flsqrt/rndu
             flsqr/rndd flsqr/rndu))

(define bij-pos-sqr (bijection-inverse bij-sqrt))

(define bij-neg-sqr
  (bijection #f negative-interval positive-interval
             flsqr/rndd flsqr/rndu
             flneg-sqrt/rndd flneg-sqrt/rndu))

(define bij-pos-recip
  (bijection #f positive-interval positive-interval
             (zeros+ flrecip/rndd) (zeros+ flrecip/rndu)
             (zeros+ flrecip/rndd) (zeros+ flrecip/rndu)))

(define bij-neg-recip
  (bijection #f negative-interval negative-interval
             (zeros- flrecip/rndd) (zeros- flrecip/rndu)
             (zeros- flrecip/rndd) (zeros- flrecip/rndu)))

(define bij-asin
  (bijection #t
             (Nonextremal-Interval -1.0 1.0 #t #t)
             (Nonextremal-Interval (* 0.5 -pi/rndd) (* 0.5 +pi/rndu) #t #t)
             flasin/rndd flasin/rndu
             flsin/rndd  flsin/rndu))

(define bij-mono-sin (bijection-inverse bij-asin))

(define bij-acos
  (bijection #f
             (Nonextremal-Interval -1.0 1.0 #t #t)
             (Nonextremal-Interval 0.0 +pi/rndu #t #t)
             flacos/rndd flacos/rndu
             flcos/rndd  flcos/rndu))

(define bij-mono-cos (bijection-inverse bij-acos))

(define bij-normal
  (bijection #t unit-interval reals
             flnormal/rndd flnormal/rndu
             flnormal-inv/rndd flnormal-inv/rndu))

(define bij-cauchy
  (bijection #t unit-interval reals
             flcauchy/rndd flcauchy/rndu
             flcauchy-inv/rndd flcauchy-inv/rndu))

(: bij-scale (-> Flonum bijection))
(define (bij-scale y)
  (when (zero? y) (raise-argument-error 'bij-scale "nonzero Flonum" y))
  (bijection (positive? y) reals reals
             (λ: ([x : Flonum]) (fl*/rndd x y))
             (λ: ([x : Flonum]) (fl*/rndu x y))
             (λ: ([z : Flonum]) (fl//rndd z y))
             (λ: ([z : Flonum]) (fl//rndu z y))))

(: bij-translate (-> Flonum bijection))
(define (bij-translate y)
  (bijection #t reals reals
             (λ: ([x : Flonum]) (fl+/rndd x y))
             (λ: ([x : Flonum]) (fl+/rndu x y))
             (λ: ([z : Flonum]) (fl-/rndd z y))
             (λ: ([z : Flonum]) (fl-/rndu z y))))
