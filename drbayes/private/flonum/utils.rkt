#lang typed/racket/base

(require math/flonum)

(provide (all-defined-out))

;; The exact sum of these is log(2) with about 159 bits precision
(define-values (log2.hi log2.lo log2.tiny)
  (values 0.6931471805599453 2.3190468138462996e-17 5.707700982493929e-34))

(: flexpm0.5 (-> Flonum Flonum))
(define (flexpm0.5 x)
  (cond [(or (<= x (fllog 0.25)) (<= 0.0 x))  (- (flexp x) 0.5)]
        [else  (* 0.5 (flexpm1 (+ (+ log2.hi x) log2.lo)))]))

(: flexpm0.5/error (-> Flonum (Values Flonum Flonum)))
(define (flexpm0.5/error x)
  (cond [(or (<= x (fllog 0.25)) (<= 0.0 x))
         (let-values ([(y.hi y.lo)  (flexp/error x)])
           (fl2- y.hi y.lo 0.5))]
        [else
         (let*-values ([(y.hi y.lo)  (fl2+ log2.hi log2.lo x)]
                       [(y.hi y.lo)  (fl2+ y.hi y.lo log2.tiny)]
                       [(y.hi y.lo)  (fl2expm1 y.hi y.lo)])
           (values (* 0.5 y.hi) (* 0.5 y.lo)))]))
