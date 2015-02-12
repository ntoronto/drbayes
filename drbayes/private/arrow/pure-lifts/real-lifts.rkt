#lang typed/racket/base

(require math/flonum
         (only-in racket/math pi)
         "../../set.rkt"
         "../../flonum.rkt"
         "../types.rkt"
         "../pure-arrows.rkt"
         "predicate-lifts.rkt"
         "make-real-lift.rkt"
         "bijection.rkt"
         "trijection.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Strictly monotone, surjective functions

(: scale/bot (-> Flonum Bot-Arrow))
(: scale/pre (-> Flonum Pre-Arrow))
(define (scale/bot y) (if (zero? y) (const/bot 0.0) ((real/bot 'scale reals reals (flscale y)))))
(define (scale/pre y) (if (zero? y) (const/pre 0.0) ((strict-monotone/pre (bij-scale y)))))

(: translate/bot (-> Flonum Bot-Arrow))
(: translate/pre (-> Flonum Pre-Arrow))
(define (translate/bot y) ((real/bot 'translate reals reals (fltranslate y))))
(define (translate/pre y) ((strict-monotone/pre (bij-translate y))))

(define-values (neg/bot neg/pre) (strict-monotone/prim 'neg flneg bij-neg))
(define-values (exp/bot exp/pre) (strict-monotone/prim 'exp flexp bij-exp))
(define-values (log/bot log/pre) (strict-monotone/prim 'log fllog bij-log))
(define-values (expm1/bot expm1/pre) (strict-monotone/prim 'expm1 flexpm1 bij-expm1))
(define-values (log1p/bot log1p/pre) (strict-monotone/prim 'log1p fllog1p bij-log1p))
(define-values (sqrt/bot sqrt/pre) (strict-monotone/prim 'sqrt flsqrt bij-sqrt))
(define-values (asin/bot asin/pre) (strict-monotone/prim 'asin flasin bij-asin))
(define-values (acos/bot acos/pre) (strict-monotone/prim 'acos flacos bij-acos))
(define-values (mono-sin/bot mono-sin/pre) (strict-monotone/prim 'mono-sin flsin bij-mono-sin))
(define-values (mono-cos/bot mono-cos/pre) (strict-monotone/prim 'mono-cos flcos bij-mono-cos))

(define pos-recip/pre (strict-monotone/pre bij-pos-recip))
(define neg-recip/pre (strict-monotone/pre bij-neg-recip))

(define pos-sqr/pre (strict-monotone/pre bij-pos-sqr))
(define neg-sqr/pre (strict-monotone/pre bij-neg-sqr))

(define-values (+/bot +/pre) (strict-monotone2d/prim '+ fl+ trij-add))
(define-values (-/bot -/pre) (strict-monotone2d/prim '- fl- trij-sub))

(define mul++/pre (strict-monotone2d/pre trij-mul++))
(define mul+-/pre (strict-monotone2d/pre trij-mul+-))
(define mul-+/pre (strict-monotone2d/pre trij-mul-+))
(define mul--/pre (strict-monotone2d/pre trij-mul--))

(define div++/pre (strict-monotone2d/pre trij-div++))
(define div+-/pre (strict-monotone2d/pre trij-div+-))
(define div-+/pre (strict-monotone2d/pre trij-div-+))
(define div--/pre (strict-monotone2d/pre trij-div--))

;; ===================================================================================================
;; Piecewise monotone, surjective functions

;; Absolute value
(define abs/bot (real/bot 'abs reals nonnegative-interval flabs))
(define (abs/pre) (ifte/pre (negative?/pre) (neg/pre) (id/pre)))

;; Square
(define sqr/bot (real/bot 'sqr reals nonnegative-interval flsqr))
(define (sqr/pre) (ifte/pre (negative?/pre) (neg-sqr/pre) (pos-sqr/pre)))

;; Reciprocal
(define recip/bot (real/bot 'recip nonzero-reals nonzero-reals flrecip))
(define (recip/pre)
  (ifte/pre (positive?/pre) (pos-recip/pre) (ifte/pre (negative?/pre) (neg-recip/pre) (fail/pre))))

;; Multiplication

(define mul-domain (set-pair reals reals))
(define */bot (real2d/bot '* reals reals reals fl*))
(define (*/pre)
  (ifte/pre (>>>/pre (fst/pre) (positive?/pre))
            (ifte/pre (>>>/pre (snd/pre) (positive?/pre))
                      (mul++/pre)
                      (ifte/pre (>>>/pre (snd/pre) (negative?/pre))
                                (mul+-/pre)
                                (>>>/pre (restrict/pre mul-domain) (const/pre 0.0))))
            (ifte/pre (>>>/pre (fst/pre) (negative?/pre))
                      (ifte/pre (>>>/pre (snd/pre) (positive?/pre))
                                (mul-+/pre)
                                (ifte/pre (>>>/pre (snd/pre) (negative?/pre))
                                          (mul--/pre)
                                          (>>>/pre (restrict/pre mul-domain) (const/pre 0.0))))
                      (>>>/pre (restrict/pre mul-domain) (const/pre 0.0)))))

;; Division

(define div-domain (set-pair reals nonzero-reals))
(define //bot (real2d/bot '/ reals nonzero-reals reals fl/))
(define (//pre)
  (ifte/pre (>>>/pre (snd/pre) (positive?/pre))
            (ifte/pre (>>>/pre (fst/pre) (positive?/pre))
                      (div++/pre)
                      (ifte/pre (>>>/pre (fst/pre) (negative?/pre))
                                (div-+/pre)
                                (>>>/pre (restrict/pre div-domain) (const/pre 0.0))))
            (ifte/pre (>>>/pre (snd/pre) (negative?/pre))
                      (ifte/pre (>>>/pre (fst/pre) (positive?/pre))
                                (div+-/pre)
                                (ifte/pre (>>>/pre (fst/pre) (negative?/pre))
                                          (div--/pre)
                                          (>>>/pre (restrict/pre div-domain) (const/pre 0.0))))
                      (fail/pre))))

#|
Sine and cosine arrows are direct translations of the following Racket functions:

(define (partial-cos x)
  (if (negative? x)
      (mono-cos (- x))
      (mono-cos x)))

(define (partial-pos-sin x)
  (if (nonpositive? (+ x (* -0.5 pi)))
      (mono-sin x)
      (let ([x  (+ x (- pi))])
        (if (nonpositive? x)
            (mono-sin (- x))
            (error 'fail)))))

(define (partial-sin x)
  (if (negative? x)
      (- (partial-pos-sin (- x)))
      (partial-pos-sin x)))

|#

;; Cosine restricted to [-π,π]
(define-syntax-rule (make-partial-cos >>> ifte mono-cos negative? neg)
  (ifte (negative?) (>>> (neg) (mono-cos)) (mono-cos)))

;; Sine restricted to [-π/2,π]
(define-syntax-rule (make-partial-pos-sin >>> ifte mono-sin translate nonpositive? neg fail)
  (ifte (>>> (translate (* -0.5 pi)) (nonpositive?))
        (mono-sin)
        (>>> (translate (- pi))
             (ifte (nonpositive?)
                   (>>> (neg) (mono-sin))
                   (fail)))))

;; Sine restricted to [-π,π]
(define-syntax-rule (make-partial-sin >>> ifte partial-pos-sin negative? neg)
  (ifte (negative?)
        (>>> (>>> (neg) (partial-pos-sin)) (neg))
        (partial-pos-sin)))

(define (partial-cos/bot) (make-partial-cos >>>/bot ifte/bot mono-cos/bot negative?/bot neg/bot))
(define (partial-cos/pre) (make-partial-cos >>>/pre ifte/pre mono-cos/pre negative?/pre neg/pre))

(define (partial-pos-sin/bot)
  (make-partial-pos-sin >>>/bot ifte/bot
                        mono-sin/bot translate/bot nonpositive?/bot neg/bot fail/bot))

(define (partial-pos-sin/pre)
  (make-partial-pos-sin >>>/pre ifte/pre
                        mono-sin/pre translate/pre nonpositive?/pre neg/pre fail/pre))

(define (partial-sin/bot)
  (make-partial-sin >>>/bot ifte/bot partial-pos-sin/bot negative?/bot neg/bot))
(define (partial-sin/pre)
  (make-partial-sin >>>/pre ifte/pre partial-pos-sin/pre negative?/pre neg/pre))
