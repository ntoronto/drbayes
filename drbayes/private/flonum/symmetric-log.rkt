#lang typed/racket/base

#|
Representing probabilities p ∈ [0,1] using symmetric logs

The typical way to increase the density of floating-point probabilities near 0 and 1 is to compute in
log space. This causes a problem when using the inverse CDF method: it extends the effective ranges of
symmetric distributions asymmetrically:

> (inv-cdf (normal-dist 0 1) -max.0 #t)
-1.8961503816218352e+154
> (inv-cdf (normal-dist 0 1) -min.0 #t)
38.46740561714434

*Symmetric logs* represent numbers p ∈ [0,1/2] using log(p), and numbers p ∈ (1/2,1] using -log(1-p).

One benefit is that -log(1-p) is an upper tail log probability, which both R and Racket's math
library's inverse CDF implementations operate on directly. The normal distribution thus has a
symmetric effective range of about [-2e154,2e154].

Another benefit is that -symlog(p) = symlog(1-p).

One drawback is that symlog(p) ∈ [-∞,-log(2)] ∪ (log(2),∞], a crazier domain than log(p) ∈ [-∞,0].

Another drawback is that multiplication and division - which in log space are just addition and
subtraction - are much more complicated.
|#

(require math/flonum
         "utils.rkt")

(provide
 flonum->flprob
 flprob->flonum
 flprob?
 flprob-normalize flprob-normalize/error
 flprob1-
 flprob* flprob*/error
 flprob/ flprob//error
 flprob+ flprob+/error
 flprob- flprob-/error
 flprob-midpoint
 )

;; ===================================================================================================
;; Conversion

(: flonum->flprob (-> Flonum Flonum))
(define (flonum->flprob x)
  (if (<= x 0.5)
      (fllog x)
      (- (fllog1p (- x)))))

(: flprob-not-nan? (-> Flonum Boolean))
(define (flprob-not-nan? x)
  (or (<= x (fllog 0.5))
      (< (fllog 2.0) x)))

(: flprob? (-> Flonum Boolean))
(define (flprob? x)
  (or (flprob-not-nan? x)
      (flnan? x)))

(: flprob->flonum (-> Flonum Flonum))
(define (flprob->flonum x)
  (cond [(not (flprob-not-nan? x))  +nan.0]
        [(< x 0.0)  (flexp x)]
        [else  (- (flexpm1 (- x)))]))

;; ===================================================================================================
;; Normalization: put any nonzero symmetric log probability in the range [-∞,-log(2)] ∪ (log(2),∞]

(: fl2prob-normalize (-> Flonum Flonum (Values Flonum Flonum)))
(define (fl2prob-normalize x.hi x.lo)
  (define x (+ x.hi x.lo))
  (cond [(flprob? x)  (values x.hi x.lo)]
        [(zero? x)    (values +inf.0 0.0)]
        [else
         (let*-values ([(x.hi x.lo sgn)  (if (> x 0.0)
                                             (values (- x.hi) (- x.lo) 1.0)
                                             (values x.hi x.lo -1.0))]
                       [(x.hi x.lo)  (fl2expm1 x.hi x.lo)]
                       [(x.hi x.lo)  (fl2log (- x.hi) (- x.lo))])
           (values (* x.hi sgn) (* x.lo sgn)))]))

(: flprob-normalize/error (-> Flonum (Values Flonum Flonum)))
(define (flprob-normalize/error x)
  (fl2prob-normalize x 0.0))

(: flprob-normalize (-> Flonum Flonum))
(define (flprob-normalize x)
  (define-values (y.hi y.lo) (flprob-normalize/error x))
  (define y (+ y.hi y.lo))
  (if (= y (fllog 2.0)) (fllog 0.5) y))

;; ===================================================================================================
;; Complement

(: flprob1- (-> Flonum Flonum))
(define (flprob1- x)
  (cond [(or (< x (fllog 0.5)) (< (fllog 2.0) x))  (- x)]
        [(= x (fllog 0.5))  x]
        [else  +nan.0]))

;; ===================================================================================================
;; Multiplication

(: flprob*/error (-> Flonum Flonum (Values Flonum Flonum)))
(define (flprob*/error x y)
  (if (not (and (flprob-not-nan? x) (flprob-not-nan? y)))
      (values +nan.0 0.0)
      (let ([x  (max x y)]
            [y  (min x y)])
        (cond [(= x +inf.0)  (values y 0.0)]  ; Multiplying by 1.0
              [(= y -inf.0)  (values y 0.0)]  ; Multiplying by 0.0
              ;; Quadrant III
              [(< x 0.0)  (fl+/error x y)]
              ;; Quadrant II/IV
              [(< y 0.0)
               (let*-values ([(z.hi z.lo)  (flexp/error (- x))]
                             [(z.hi z.lo)  (fl2log1p (- z.hi) (- z.lo))])
                 (fl2+ z.hi z.lo y))]
              ;; Quadrant I
              [else
               (let*-values ([(a.hi a.lo)  (flexpm1/error (- x))]
                             [(a.hi a.lo)  (fl2log (- a.hi) (- a.lo))]
                             [(b.hi b.lo)  (flexpm1/error (- y))]
                             [(b.hi b.lo)  (fl2log (- b.hi) (- b.lo))]
                             [(z.hi z.lo)  (fl2+ a.hi a.lo b.hi b.lo)])
                 (cond [(fl2<= z.hi z.lo (fllog 0.5) 0.0)
                        (values z.hi z.lo)]
                       [(< x 80.0)
                        (let*-values ([(a.hi a.lo)  (flexpm1/error x)]
                                      [(b.hi b.lo)  (flexp/error y)]
                                      [(z.hi z.lo)  (fl2+ a.hi a.lo b.hi b.lo)]
                                      [(z.hi z.lo)  (fl2log z.hi z.lo)]
                                      [(z.hi z.lo)  (fl2- z.hi z.lo x)]
                                      [(z.hi z.lo)  (fl2- z.hi z.lo y)])
                          (values (- z.hi) (- z.lo)))]
                       [else
                        ;; At this point, (flexpm1 x) = (flexp x).
                        ;; The following code is derived from the case above using that fact, to
                        ;; avoid premature overflow.
                        (let*-values ([(z.hi z.lo)  (fl-/error y x)]
                                      [(z.hi z.lo)  (fl2exp z.hi z.lo)]
                                      [(z.hi z.lo)  (fl2log1p z.hi z.lo)]
                                      [(z.hi z.lo)  (fl2- z.hi z.lo y)])
                          (values (- z.hi) (- z.lo)))]))]))))

(: flprob* (-> Flonum Flonum Flonum))
(define (flprob* x y)
  (define-values (z.hi z.lo) (flprob*/error x y))
  (define z (+ z.hi z.lo))
  (if (= z (fllog 2.0)) (fllog 0.5) z))

;; ===================================================================================================
;; Division

(: flprob//error (-> Flonum Flonum (Values Flonum Flonum)))
(define (flprob//error x y)
  (cond [(not (and (flprob-not-nan? x) (flprob-not-nan? y)))  (values +nan.0 0.0)]
        [(< y x)  (values +nan.0 0.0)]       ; Result is > 1
        [(= y -inf.0)  (values +nan.0 0.0)]  ; Dividing by 0
        [(= x -inf.0)  (values x 0.0)]       ; Dividing 0
        [(= x y)  (values +inf.0 0.0)]       ; Dividing equal numbers
        ;; Quadrant III
        [(< y 0.0)
         (let-values ([(z.hi z.lo)  (fl-/error x y)])
           (if (fl2<= z.hi z.lo (fllog 0.5) 0.0)
               (values z.hi z.lo)
               (let*-values ([(z.hi z.lo)  (fl2expm1 z.hi z.lo)]
                             [(z.hi z.lo)  (fl2log (- z.hi) (- z.lo))])
                 (values (- z.hi) (- z.lo)))))]
        ;; Quadrant II
        [(< x 0.0)
         (let*-values ([(z.hi z.lo)  (flexp/error (- y))]
                       [(z.hi z.lo)  (fl2log1p (- z.hi) (- z.lo))]
                       [(z.hi z.lo)  (fl2+ (- z.hi) (- z.lo) x)])
           (if (fl2<= z.hi z.lo (fllog 0.5) 0.0)
               (values z.hi z.lo)
               (let*-values ([(z.hi z.lo)  (fl2exp z.hi z.lo)]
                             [(z.hi z.lo)  (fl2log1p (- z.hi) (- z.lo))])
                 (values (- z.hi) (- z.lo)))))]
        ;; Quadrant I
        [(or (< y 80.0) (> (- x y) -80.0))
         (define-values (a.hi a.lo) (fl-/error x y))
         (let*-values ([(b.hi b.lo)  (flexpm1/error y)]
                       [(c.hi c.lo)  (fl2expm1 a.hi a.lo)]
                       [(b.hi b.lo)  (fl2/ b.hi b.lo c.hi c.lo)]
                       [(b.hi b.lo)  (fl2log (- b.hi) (- b.lo))])
           (fl2+ a.hi a.lo b.hi b.lo))]
        [else
         ;; At this point, (flexpm1/error y) = (flexp/error y) and (flexpm1 (- x y)) ≈ -1.
         ;; The following code is derived from the case above using those facts, to avoid
         ;; premature overflow.
         (values x 0.0)]))

(: flprob/ (-> Flonum Flonum Flonum))
(define (flprob/ x y)
  (define-values (z.hi z.lo) (flprob//error x y))
  (define z (+ z.hi z.lo))
  (if (= z (fllog 2.0)) (fllog 0.5) z))

;; ===================================================================================================
;; Addition

(: flprob+/error (-> Flonum Flonum (Values Flonum Flonum)))
(define (flprob+/error x y)
  (cond [(not (and (flprob-not-nan? x) (flprob-not-nan? y)))  (values +nan.0 0.0)]
        [(> x (- y))  (values +nan.0 0.0)]  ; Result > 1
        [(= x (- y))  (values +inf.0 0.0)]  ; Result = 1
        [else
         (let ([x  (max x y)]
               [y  (min x y)])
           (cond [(= x -inf.0)  (values -inf.0 0.0)]  ; Result 0 + 0 = 0
                 ;; Quadrant III
                 [(< x 0.0)
                  (define-values (z.hi z.lo)
                    (let*-values ([(z.hi z.lo)  (fl-/error y x)]
                                  [(z.hi z.lo)  (fl2exp z.hi z.lo)]
                                  [(z.hi z.lo)  (fl2log1p z.hi z.lo)])
                      (fl2+ z.hi z.lo x)))
                  (if (fl2<= z.hi z.lo (fllog 0.5) 0.0)
                      (values z.hi z.lo)
                      (let*-values ([(a.hi a.lo)  (flexpm0.5/error x)]
                                    [(b.hi b.lo)  (flexpm0.5/error y)]
                                    [(z.hi z.lo)  (fl2+ a.hi a.lo b.hi b.lo)]
                                    [(z.hi z.lo)  (fl2log (- z.hi) (- z.lo))])
                        (values (- z.hi) (- z.lo))))]
                 ;; Quadrants II and IV
                 [else
                  (let*-values ([(z.hi z.lo)  (fl+/error x y)]
                                [(z.hi z.lo)  (fl2exp z.hi z.lo)]
                                [(z.hi z.lo)  (fl2log1p (- z.hi) (- z.lo))])
                    (fl2+ (- z.hi) (- z.lo) x))]))]))

(: flprob+ (-> Flonum Flonum Flonum))
(define (flprob+ x y)
  (define-values (z.hi z.lo) (flprob+/error x y))
  (define z (+ z.hi z.lo))
  (if (= z (fllog 2.0)) (fllog 0.5) z))

;; ===================================================================================================
;; Subtraction

(: flprob-/error (-> Flonum Flonum (Values Flonum Flonum)))
(define (flprob-/error x y)
  (cond [(not (and (flprob-not-nan? x) (flprob-not-nan? y)))  (values +nan.0 0.0)]
        [(< x y)  (values +nan.0 0.0)]             ; Result would be negative
        [(= x y)  (values -inf.0 0.0)]             ; Subtracting same number
        [(= x +inf.0)  (values (flprob1- y) 0.0)]  ; Subtracting from 1
        [(= y -inf.0)  (values x 0.0)]             ; Subtracting 0
        ;; Quadrant III
        [(< x 0.0)
         (let*-values ([(z.hi z.lo)  (fl-/error y x)]
                       [(z.hi z.lo)  (fl2expm1 z.hi z.lo)]
                       [(z.hi z.lo)  (fl2log (- z.hi) (- z.lo))])
           (fl2+ z.hi z.lo x))]
        ;; Quadrant IV
        [(< y 0.0)
         (define-values (z.hi z.lo)
           (let*-values ([(a.hi a.lo)  (flexpm0.5/error (- x))]
                         [(b.hi b.lo)  (flexpm0.5/error y)]
                         [(z.hi z.lo)  (fl2+ a.hi a.lo b.hi b.lo)]
                         [(z.hi z.lo)  (cond [(fl2positive? z.hi z.lo)  (values 0.0 0.0)]
                                             [else                      (values z.hi z.lo)])])
             (fl2log (- z.hi) (- z.lo))))
         (if (<= (+ z.hi z.lo) (fllog 0.5))
             (values z.hi z.lo)
             (let ([x  (max (- x) y)]
                   [y  (min (- x) y)])
               (let*-values ([(z.hi z.lo)  (fl-/error y x)]
                             [(z.hi z.lo)  (fl2exp z.hi z.lo)]
                             [(z.hi z.lo)  (fl2log1p z.hi z.lo)]
                             [(z.hi z.lo)  (fl2+ z.hi z.lo x)])
                 (values (- z.hi) (- z.lo)))))]
        ;; Quadrant I
        [else
         (let*-values ([(z.hi z.lo)  (fl-/error y x)]
                       [(z.hi z.lo)  (fl2expm1 z.hi z.lo)]
                       [(z.hi z.lo)  (fl2log (- z.hi) (- z.lo))])
           (fl2- z.hi z.lo y))]))

(: flprob- (-> Flonum Flonum Flonum))
(define (flprob- x y)
  (define-values (z.hi z.lo) (flprob-/error x y))
  (define z (+ z.hi z.lo))
  (if (= z (fllog 2.0)) (fllog 0.5) z))

;; ===================================================================================================
;; Midpoint

(: flprob-midpoint (-> Flonum Flonum Flonum))
;; Greatest observed error is 1.5252 ulps
(define (flprob-midpoint x y)
  (cond
    [(not (and (flprob-not-nan? x) (flprob-not-nan? y)))  +nan.0]
    [else
     (: z Flonum)
     (define z
       (let loop ([x x] [y y])
         (cond [(> x (- y))  (- (loop (- x) (- y)))]
               [(= x y)  x]
               [(= x (- y))  (fllog 0.5)]
               [else
                (let ([x  (max x y)]
                      [y  (min x y)])
                  (cond [(< x 0.0)
                         (+ (fllog 0.5) (+ x (fllog1p (flexp (fl- y x)))))]
                        [else
                         (+ (fllog 0.5) (fllog1p (- (flexp y) (flexp (- x)))))]))])))
     (if (= z (fllog 2.0)) (fllog 0.5) z)]))
