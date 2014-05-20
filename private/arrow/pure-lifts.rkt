#lang typed/racket/base

(require racket/promise
         (only-in racket/math pi)
         math/flonum
         math/distributions
         "../set.rkt"
         "pure-arrows.rkt"
         "preimage-mapping.rkt"
         "directed-rounding-flops.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; R -> R lifts

(: real/bot (Symbol Nonempty-Real-Set Nonempty-Real-Set (Flonum -> Flonum) -> Bot-Arrow))
(define (real/bot name X Y f)
  (: arg-error (Value -> Bottom))
  (define (arg-error a)
    (bottom (delay (format "~a: expected argument in ~e; given ~e" name X a))))
  
  (: res-error (Flonum -> Bottom))
  (define (res-error b)
    (bottom (delay (format "~a: expected result in ~e; produced ~e" name Y b))))
  
  (λ (a)
    (cond [(and (flonum? a) (real-set-member? X a))
           (define b (f a))
           (cond [(real-set-member? Y b)  b]
                 [else
                  (res-error b)])]
          [else
           (arg-error a)])))

;; ---------------------------------------------------------------------------------------------------
;; Strictly monotone (i.e. invertible)

(: strict-monotone-apply (Boolean (Flonum -> Flonum) (Flonum -> Flonum) Set -> Set))
(define (strict-monotone-apply inc? f/rndd f/rndu A)
  (define B
    (real-set-map (λ (A)
                    (define-values (a1 a2 a1? a2?) (interval-fields A))
                    (cond [inc?  (interval (f/rndd a1) (f/rndu a2) a1? a2?)]
                          [else  (interval (f/rndd a2) (f/rndu a1) a2? a1?)]))
                  (set-take-reals A)))
  (if (empty-real-set? B) empty-set B))

(: strict-monotone/pre (Nonempty-Real-Set
                        Nonempty-Real-Set
                        Boolean
                        (Flonum -> Flonum) (Flonum -> Flonum)
                        (Flonum -> Flonum) (Flonum -> Flonum)
                        -> Pre-Arrow))
(define ((strict-monotone/pre X Y inc? f/rndd f/rndu g/rndd g/rndu) A)
  (let ([A  (set-intersect A X)])
    (pre-mapping (set-intersect Y (strict-monotone-apply inc? f/rndd f/rndu A))
                 (λ (B) (set-intersect A (strict-monotone-apply inc? g/rndd g/rndu B))))))

(: strict-monotone/prim (Symbol
                         Nonempty-Real-Set
                         Nonempty-Real-Set
                         (Flonum -> Flonum)
                         Boolean
                         (Flonum -> Flonum) (Flonum -> Flonum)
                         (Flonum -> Flonum) (Flonum -> Flonum)
                         -> (Values Bot-Arrow Pre-Arrow)))
(define (strict-monotone/prim name X Y f inc? f/rndd f/rndu g/rndd g/rndu)
  (values (real/bot name X Y f)
          (strict-monotone/pre X Y inc? f/rndd f/rndu g/rndd g/rndu)))

;; ---------------------------------------------------------------------------------------------------
;; Non-strictly monotone

(: monotone-apply ((Nonempty-Interval -> Real-Set) Set -> Set))
(define (monotone-apply f A)
  (define B (real-set-map f (set-take-reals A)))
  (if (empty-real-set? B) empty-set B))

(: monotone/pre (Nonempty-Real-Set
                 Nonempty-Real-Set
                 (Nonempty-Interval -> Real-Set)
                 (Nonempty-Interval -> Real-Set)
                 -> Pre-Arrow))
(define ((monotone/pre X Y img pre) A)
  (let ([A  (set-intersect A X)])
    (pre-mapping (set-intersect Y (monotone-apply img A))
                 (λ (B) (set-intersect A (monotone-apply pre B))))))

(: monotone/prim (Symbol
                  Nonempty-Real-Set
                  Nonempty-Real-Set
                  (Flonum -> Flonum)
                  (Nonempty-Interval -> Real-Set)
                  (Nonempty-Interval -> Real-Set)
                  -> (Values Bot-Arrow Pre-Arrow)))
(define (monotone/prim name X Y f img pre)
  (values (real/bot name X Y f)
          (monotone/pre X Y img pre)))

;; ===================================================================================================
;; R x R -> R lifts

(: real2d/bot (Symbol Nonempty-Real-Set Nonempty-Real-Set Nonempty-Real-Set (Flonum Flonum -> Flonum)
                      -> Bot-Arrow))
(define (real2d/bot name X1 X2 Y f)
  (: arg-error (Value -> Bottom))
  (define (arg-error a)
    (bottom (delay (format "~a: expected argument in ~e; given ~e" name (set-pair X1 X2) a))))
  
  (: res-error (Flonum -> Bottom))
  (define (res-error b)
    (bottom (delay (format "~a: expected result in ~e; produced ~e" name Y b))))
  
  (λ (a)
    (cond [(pair? a)
           (define a1 (car a))
           (define a2 (cdr a))
           (cond [(and (flonum? a1) (flonum? a2) (real-set-member? X1 a1) (real-set-member? X2 a2))
                  (define b (f a1 a2))
                  (cond [(real-set-member? Y b)  b]
                        [else
                         (res-error b)])]
                 [else
                  (arg-error a)])]
          [else
           (arg-error a)])))

;; ---------------------------------------------------------------------------------------------------
;; Strictly monotone

(: strict-monotone2d-apply (Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                                    Nonempty-Interval Nonempty-Interval -> Interval))
(define (strict-monotone2d-apply xinc? yinc? f/rndd f/rndu X Y)
  (let-values ([(x1 x2 x1? x2?)  (let-values ([(x1 x2 x1? x2?)  (interval-fields X)])
                                   (cond [xinc?  (values x1 x2 x1? x2?)]
                                         [else   (values x2 x1 x2? x1?)]))]
               [(y1 y2 y1? y2?)  (let-values ([(y1 y2 y1? y2?)  (interval-fields Y)])
                                   (cond [yinc?  (values y1 y2 y1? y2?)]
                                         [else   (values y2 y1 y2? y1?)]))])
    (interval (f/rndd x1 y1) (f/rndu x2 y2) (and x1? y1?) (and x2? y2?))))

(: strict-monotone2d-image (Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                                    Nonempty-Set Nonempty-Set -> Set))
(define (strict-monotone2d-image xinc? yinc? f/rndd f/rndu A Y)
  (define-values (A1 A2) (set-projs A))
  (let ([A1  (set-take-reals A1)]
        [A2  (set-take-reals A2)])
    (define B
      (real-set-map
       (λ (A1) (real-set-map (λ (A2) (strict-monotone2d-apply xinc? yinc? f/rndd f/rndu A1 A2)) A2))
       A1))
    (cond [(empty-real-set? B)  empty-set]
          [else  (set-intersect Y B)])))

(: strict-monotone2d-preimage
   (Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
            Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
            Nonempty-Set Nonempty-Set -> Set))
(define (strict-monotone2d-preimage gz? gy? g/rndd g/rndu hz? hx? h/rndd h/rndu X B)
  (define-values (X1 X2) (set-projs X))
  (let ([B   (set-take-reals B)]
        [X1  (set-take-reals X1)]
        [X2  (set-take-reals X2)])
    (define A1
      (real-set-map
       (λ (B) (real-set-map (λ (X2) (strict-monotone2d-apply gz? gy? g/rndd g/rndu B X2)) X2))
       B))
    (define A2
      (real-set-map
       (λ (B) (real-set-map (λ (X1) (strict-monotone2d-apply hz? hx? h/rndd h/rndu B X1)) X1))
       B))
    (set-intersect X (set-pair (bot-basic A1) (bot-basic A2)))))

(: strict-monotone2d/pre (Nonempty-Real-Set
                          Nonempty-Real-Set
                          Nonempty-Real-Set
                          Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                          Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                          Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                          -> Pre-Arrow))
(define (strict-monotone2d/pre X1 X2 Y
                               fx? fy? f/rndd f/rndu
                               gz? gy? g/rndd g/rndu
                               hz? hx? h/rndd h/rndu)
  (define X (set-pair X1 X2))
  (λ (A)
    (let ([A  (set-intersect A X)])
      (cond [(empty-set? A)  empty-pre-mapping]
            [else  (pre-mapping (strict-monotone2d-image fx? fy? f/rndd f/rndu A Y)
                                (λ (B) (strict-monotone2d-preimage gz? gy? g/rndd g/rndu
                                                                   hz? hx? h/rndd h/rndu
                                                                   A B)))]))))

(: strict-monotone2d/prim (Symbol
                           Nonempty-Real-Set Nonempty-Real-Set Nonempty-Real-Set
                           (Flonum Flonum -> Flonum)
                           Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                           Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                           Boolean Boolean (Flonum Flonum -> Flonum) (Flonum Flonum -> Flonum)
                           -> (Values Bot-Arrow Pre-Arrow)))
(define (strict-monotone2d/prim name X1 X2 Y
                                f
                                fx? fy? f/rndd f/rndu
                                gz? gy? g/rndd g/rndu
                                hz? hx? h/rndd h/rndu)
  (values (real2d/bot name X1 X2 Y f)
          (strict-monotone2d/pre X1 X2 Y
                                 fx? fy? f/rndd f/rndu
                                 gz? gy? g/rndd g/rndu
                                 hz? hx? h/rndd h/rndu)))

;; ===================================================================================================
;; Predicate lifts

(: predicate/bot (Symbol (Value -> (U Bottom Boolean)) Nonempty-Set Nonempty-Set -> Bot-Arrow))
(define ((predicate/bot name f Xt Xf) a)
  (define b (f a))
  (cond [(bottom? b)  b]
        [(eq? b #t)  (cond [(set-member? Xt a)  #t]
                           [else  (bottom (delay (format "~a: expected argument in ~e; given ~e"
                                                         name Xt a)))])]
        [(eq? b #f)  (cond [(set-member? Xf a)  #f]
                           [else  (bottom (delay (format "~a: expected argument in ~e; given ~e"
                                                         name Xf a)))])]
        [else  (bottom (delay (format "~a: expected Boolean condition; given ~e" name b)))]))

(: predicate/pre (Nonempty-Set Nonempty-Set -> Pre-Arrow))
(define ((predicate/pre Xt Xf) A)
  (define At (set-intersect A Xt))
  (define Af (set-intersect A Xf))
  (cond [(and (empty-set? At) (empty-set? Af))  empty-pre-mapping]
        [(empty-set? Af)  (nonempty-pre-mapping trues  (λ (B) At))]
        [(empty-set? At)  (nonempty-pre-mapping falses (λ (B) Af))]
        [else   (define A (delay (set-join At Af)))
                (nonempty-pre-mapping bools (λ (B) (cond [(trues? B)  At]
                                                         [(falses? B)  Af]
                                                         [else  (force A)])))]))

(: predicate/prim (Symbol (Value -> (U Bottom Boolean)) Nonempty-Set Nonempty-Set
                          -> (Values Bot-Arrow Pre-Arrow)))
(define (predicate/prim name f Xt Xf)
  (values (predicate/bot name f Xt Xf)
          (predicate/pre Xt Xf)))

;; ===================================================================================================
;; Tagged value lifts

;; ---------------------------------------------------------------------------------------------------
;; Tag predicate lifts

(: tag?/bot (Tag -> Bot-Arrow))
(define ((tag?/bot tag) a) (and (tagged-value? a) (eq? tag (tagged-value-tag a))))

(: tag?/pre (Tag -> Pre-Arrow))
(define (tag?/pre tag) (predicate/pre (bot-tagged tag universe) (top-tagged tag empty-set)))

;; ---------------------------------------------------------------------------------------------------
;; Tagging lifts

(: tag/bot (Tag -> Bot-Arrow))
(define ((tag/bot tag) a) (tagged-value tag a))

(: tag/pre (Tag -> Pre-Arrow))
(define ((tag/pre tag) A) (nonempty-pre-mapping (set-tag A tag) (λ (B) (set-untag B tag))))

;; ---------------------------------------------------------------------------------------------------
;; Untagging lifts

(: untag/bot (Tag -> Bot-Arrow))
(define ((untag/bot tag) a)
  (if (and (tagged-value? a) (eq? tag (tagged-value-tag a)))
      (tagged-value-value a)
      (bottom (delay (format "expected ~a; given ~e" tag a)))))

(: untag/pre (Tag -> Pre-Arrow))
(define ((untag/pre tag) A)
  (pre-mapping (set-untag A tag) (λ (B) (set-tag B tag))))

;; ===================================================================================================
;; Computable lifts

;; ---------------------------------------------------------------------------------------------------
;; Data type predicates

(define-values (real?/bot real?/pre) (predicate/prim 'real? flonum? reals not-reals))
(define-values (null?/bot null?/pre) (predicate/prim 'null? null? nulls not-nulls))
(define-values (pair?/bot pair?/pre) (predicate/prim 'pair? pair? pairs not-pairs))
(define-values (boolean?/bot boolean?/pre) (predicate/prim 'boolean? boolean? bools not-bools))

;; ---------------------------------------------------------------------------------------------------
;; Monotone elementary R -> R functions

(: scale/bot (Flonum -> Bot-Arrow))
(define (scale/bot y)
  (cond [(fl= y 0.0)  (const/bot 0.0)]
        [else  (real/bot 'scale reals reals (λ: ([x : Flonum]) (fl* x y)))]))

(: scale/pre (Flonum -> Pre-Arrow))
(define (scale/pre y)
  (cond [(fl= y 0.0)  (const/pre 0.0)]
        [else  (strict-monotone/pre
                reals reals
                (y . fl> . 0.0)
                (λ: ([x : Flonum]) (fl*/rndd x y))
                (λ: ([x : Flonum]) (fl*/rndu x y))
                (λ: ([z : Flonum]) (fl//rndd z y))
                (λ: ([z : Flonum]) (fl//rndu z y)))]))

(: translate/bot (Flonum -> Bot-Arrow))
(define (translate/bot y)
  (real/bot 'translate reals reals (λ: ([x : Flonum]) (fl+ x y))))

(: translate/pre (Flonum -> Pre-Arrow))
(define (translate/pre y)
  (strict-monotone/pre
   reals reals #t
   (λ: ([x : Flonum]) (fl+/rndd x y))
   (λ: ([x : Flonum]) (fl+/rndu x y))
   (λ: ([z : Flonum]) (fl-/rndd z y))
   (λ: ([z : Flonum]) (fl-/rndu z y))))

(: flneg (Flonum -> Flonum))
(define (flneg x) (fl* -1.0 x))

(: flsqr (Flonum -> Flonum))
(define (flsqr x) (fl* x x))

(: flrecip (Flonum -> Flonum))
(define (flrecip x) (fl/ 1.0 x))

(: flneg-sqrt/rndd (Flonum -> Flonum))
(define (flneg-sqrt/rndd x) (flneg (flsqrt/rndu x)))

(: flneg-sqrt/rndu (Flonum -> Flonum))
(define (flneg-sqrt/rndu x) (flneg (flsqrt/rndd x)))

(define-values (neg/bot neg/pre)
  (strict-monotone/prim 'neg reals reals flneg #f flneg flneg flneg flneg))

(define-values (exp/bot exp/pre)
  (strict-monotone/prim
   'exp reals nonnegative-interval
   flexp
   #t
   flexp/rndd
   flexp/rndu
   fllog/rndd
   fllog/rndu))

(define-values (log/bot log/pre)
  (strict-monotone/prim
   'log nonnegative-interval reals
   fllog
   #t
   fllog/rndd
   fllog/rndu
   flexp/rndd
   flexp/rndu))

(define-values (sqrt/bot sqrt/pre)
  (strict-monotone/prim
   'sqrt nonnegative-interval nonnegative-interval
   flsqrt
   #t
   flsqrt/rndd
   flsqrt/rndu
   flsqr/rndd
   flsqr/rndu))

(define-values (asin/bot asin/pre)
  (strict-monotone/prim
   'asin
   (Nonextremal-Interval -1.0 1.0 #t #t)
   (Nonextremal-Interval -pi/2/rndd pi/2/rndu #t #t)
   flasin
   #t
   flasin/rndd
   flasin/rndu
   flsin/rndd
   flsin/rndu))

(define-values (acos/bot acos/pre)
  (strict-monotone/prim
   'acos
   (Nonextremal-Interval -1.0 1.0 #t #t)
   (Nonextremal-Interval 0.0 pi/rndu #t #t)
   flacos
   #f
   flacos/rndd
   flacos/rndu
   flcos/rndd
   flcos/rndu))

(define-values (mono-sin/bot mono-sin/pre)
  (strict-monotone/prim
   'mono-sin
   (Nonextremal-Interval -pi/2/rndd pi/2/rndu #t #t)
   (Nonextremal-Interval -1.0 1.0 #t #t)
   flsin
   #t
   flsin/rndd
   flsin/rndu
   flasin/rndd
   flasin/rndu))

(define-values (mono-cos/bot mono-cos/pre)
  (strict-monotone/prim
   'mono-cos
   (Nonextremal-Interval 0.0 pi/rndu #t #t)
   (Nonextremal-Interval -1.0 1.0 #t #t)
   flcos
   #f
   flcos/rndd
   flcos/rndu
   flacos/rndd
   flacos/rndu))

(define pos-recip/pre
  (strict-monotone/pre
   positive-interval positive-interval #f
   flrecip/rndd
   flrecip/rndu
   flrecip/rndd
   flrecip/rndu))

(define neg-recip/pre
  (strict-monotone/pre
   negative-interval negative-interval #f
   flrecip/rndd
   flrecip/rndu
   flrecip/rndd
   flrecip/rndu))

(define pos-sqr/pre
  (strict-monotone/pre
   nonnegative-interval nonnegative-interval #t
   flsqr/rndd
   flsqr/rndu
   flsqrt/rndd
   flsqrt/rndu))

(define neg-sqr/pre
  (strict-monotone/pre
   negative-interval positive-interval #f
   flsqr/rndd
   flsqr/rndu
   flneg-sqrt/rndd
   flneg-sqrt/rndu))

(: monotone-interval-img ((Flonum -> Flonum) -> (Nonempty-Interval -> Interval)))
(define ((monotone-interval-img f) A)
  (define-values (a1 a2 a1? a2?) (interval-fields A))
  (interval (f a1) (f a2) #t #t))

(define flfloor-img (monotone-interval-img flfloor))
(define flceiling-img (monotone-interval-img flceiling))
(define flround-img (monotone-interval-img flround))
(define fltruncate-img (monotone-interval-img fltruncate))

(: flfloor-pre (Nonempty-Interval -> Interval))
(define (flfloor-pre B)
  (define-values (b1 b2 b1? b2?) (interval-fields B))
  (interval (if b1? (flceiling b1) (fl+/rndd (flfloor b1) 1.0))
            (if b2? (fl+/rndu (flfloor b2) 1.0) (flceiling b2))
            #t
            #f))

(: flceiling-pre (Nonempty-Interval -> Interval))
(define (flceiling-pre B)
  (define-values (b1 b2 b1? b2?) (interval-fields B))
  (interval (if b1? (fl-/rndd (flceiling b1) 1.0) (flfloor b1))
            (if b2? (flfloor b2) (fl-/rndu (flceiling b2) 1.0))
            #f
            #t))

(: flround-pre (Nonempty-Interval -> Interval))
(define (flround-pre B)
  (define-values (b1 b2 b1? b2?) (interval-fields B))
  (interval (if b1? (fl-/rndd (flceiling b1) 0.5) (fl+/rndd (flfloor b1) 0.5))
            (if b2? (fl+/rndu (flfloor b2) 0.5) (fl-/rndu (flceiling b2) 0.5))
            (if b1? (fleven? (flceiling b1)) (flodd? (flfloor b1)))
            (if b2? (fleven? (flfloor b2)) (flodd? (flceiling b2)))))

(: fltruncate-pre (Nonempty-Interval -> Interval))
(define (fltruncate-pre B)
  (define-values (b1 b2 b1? b2?) (interval-fields B))
  (define-values (a1 a1?)
    (cond [b1?   (let* ([a1   (flceiling b1)]
                        [a1?  (b1 . fl> . 0.0)])
                   (values (if a1? a1 (fl-/rndd a1 1.0)) a1?))]
          [else  (let* ([a1   (flfloor b1)]
                        [a1?  (a1 . fl> . -1.0)])
                   (values (if a1? (fl+/rndd a1 1.0) a1) a1?))]))
  (define-values (a2 a2?)
    (cond [b2?   (let* ([a2   (flfloor b2)]
                        [a2?  (a2 . fl< . 0.0)])
                   (values (if a2? a2 (fl+/rndu a2 1.0)) a2?))]
          [else  (let* ([a2  (flceiling b2)]
                        [a2?  (a2 . fl< . 1.0)])
                   (values (if a2? (fl-/rndu a2 1.0) a2) a2?))]))
  (interval a1 a2 a1? a2?))

(define-values (floor/bot floor/pre)
  (monotone/prim 'floor reals reals flfloor flfloor-img flfloor-pre))

(define-values (ceiling/bot ceiling/pre)
  (monotone/prim 'ceiling reals reals flceiling flceiling-img flceiling-pre))

(define-values (round/bot round/pre)
  (monotone/prim 'round reals reals flround flround-img flround-pre))

(define-values (truncate/bot truncate/pre)
  (monotone/prim 'truncate reals reals fltruncate fltruncate-img fltruncate-pre))

;; ---------------------------------------------------------------------------------------------------
;; Inverse CDFs

(: inverse-cdf/prim (Symbol Nonempty-Interval (Flonum -> Flonum) Index (Flonum -> Flonum) Index
                            -> (Values Bot-Arrow Pre-Arrow)))
(define (inverse-cdf/prim name Y inv-cdf inv-ulp-error cdf ulp-error)
  (define-values (a1 a2 _a1? _a2?) (interval-fields Y))
  (define-values (inv-cdf/rndd inv-cdf/rndu)
    (make-unary-flops/fake-rnd inv-cdf inv-ulp-error inv-ulp-error a1 a2))
  (define-values (cdf/rndd cdf/rndu)
    (make-unary-flops/fake-rnd cdf ulp-error ulp-error 0.0 1.0))
  (strict-monotone/prim name unit-interval Y inv-cdf #t inv-cdf/rndd inv-cdf/rndu cdf/rndd cdf/rndu))


(: cauchy-inv-cdf (Flonum -> Flonum))
(define (cauchy-inv-cdf p)
  (flcauchy-inv-cdf 0.0 1.0 p #f #f))

(: cauchy-cdf (Flonum -> Flonum))
(define (cauchy-cdf x)
  (flcauchy-cdf 0.0 1.0 x #f #f))

(define-values (cauchy/bot cauchy/pre)
  (inverse-cdf/prim 'cauchy reals cauchy-inv-cdf 2 cauchy-cdf 2))


(: normal-inv-cdf (Flonum -> Flonum))
(define (normal-inv-cdf p)
  (flnormal-inv-cdf 0.0 1.0 p #f #f))

(: normal-cdf (Flonum -> Flonum))
(define (normal-cdf x)
  (flnormal-cdf 0.0 1.0 x #f #f))

(define-values (normal/bot normal/pre)
  (inverse-cdf/prim 'normal reals normal-inv-cdf 4 normal-cdf 4))

;; ---------------------------------------------------------------------------------------------------
;; Monotone arithmetic R x R -> R functions

(define-values (+/bot +/pre)
  (strict-monotone2d/prim
   '+ reals reals reals
   fl+
   #t #t fl+/rndd fl+/rndu
   #t #f fl-/rndd fl-/rndu
   #t #f fl-/rndd fl-/rndu))

(: neg-fl-/rndd (Flonum Flonum -> Flonum))
(define (neg-fl-/rndd z x) (fl-/rndd x z))

(: neg-fl-/rndu (Flonum Flonum -> Flonum))
(define (neg-fl-/rndu z x) (fl-/rndu x z))

(define-values (-/bot -/pre)
  (strict-monotone2d/prim
   '- reals reals reals
   fl-
   #t #f fl-/rndd fl-/rndu
   #t #t fl+/rndd fl+/rndu
   #f #t neg-fl-/rndd neg-fl-/rndu))

(define pos-pos-mul/pre
  (strict-monotone2d/pre
   nonnegative-interval nonnegative-interval nonnegative-interval
   #t #t fl*/rndd fl*/rndu
   #t #f fl//rndd fl//rndu
   #t #f fl//rndd fl//rndu))

(define pos-neg-mul/pre
  (strict-monotone2d/pre
   nonnegative-interval negative-interval nonpositive-interval
   #f #t fl*/rndd fl*/rndu
   #f #t fl//rndd fl//rndu
   #t #t fl//rndd fl//rndu))

(define neg-pos-mul/pre
  (strict-monotone2d/pre
   negative-interval nonnegative-interval nonpositive-interval
   #t #f fl*/rndd fl*/rndu
   #t #t fl//rndd fl//rndu
   #f #t fl//rndd fl//rndu))

(define neg-neg-mul/pre
  (strict-monotone2d/pre
   negative-interval negative-interval positive-interval
   #f #f fl*/rndd fl*/rndu
   #f #f fl//rndd fl//rndu
   #f #f fl//rndd fl//rndu))

(: recip-fl//rndd (Flonum Flonum -> Flonum))
(define (recip-fl//rndd z x) (fl//rndd x z))

(: recip-fl//rndu (Flonum Flonum -> Flonum))
(define (recip-fl//rndu z x) (fl//rndu x z))

(define pos-pos-div/pre
  (strict-monotone2d/pre
   positive-interval positive-interval positive-interval
   #t #f fl//rndd fl//rndu
   #t #t fl*/rndd fl*/rndu
   #f #t recip-fl//rndd recip-fl//rndu))

(define pos-neg-div/pre
  (strict-monotone2d/pre
   positive-interval negative-interval negative-interval
   #f #f fl//rndd fl//rndu
   #f #f fl*/rndd fl*/rndu
   #f #f recip-fl//rndd recip-fl//rndu))

(define neg-pos-div/pre
  (strict-monotone2d/pre
   negative-interval positive-interval negative-interval
   #t #t fl//rndd fl//rndu
   #t #f fl*/rndd fl*/rndu
   #t #f recip-fl//rndd recip-fl//rndu))

(define neg-neg-div/pre
  (strict-monotone2d/pre
   negative-interval negative-interval positive-interval
   #f #t fl//rndd fl//rndu
   #f #t fl*/rndd fl*/rndu
   #t #t recip-fl//rndd recip-fl//rndu))

;; ---------------------------------------------------------------------------------------------------
;; Real predicates

(: real-predicate/prim (Symbol (Flonum -> Boolean) Nonempty-Real-Set Nonempty-Real-Set
                               -> (Values Bot-Arrow Pre-Arrow)))
(define (real-predicate/prim name p? At Af)
  (predicate/prim
   name
   (λ (a) (if (flonum? a) (p? a) (bottom (delay (format "~a: expected Flonum; given ~e" a)))))
   (bot-basic At)
   (bot-basic Af)))

(define-values (negative?/bot negative?/pre)
  (real-predicate/prim 'negative?
                       (λ: ([x : Flonum]) (x . fl< . 0.0))
                       negative-interval
                       nonnegative-interval))

(define-values (positive?/bot positive?/pre)
  (real-predicate/prim 'positive?
                       (λ: ([x : Flonum]) (x . fl> . 0.0))
                       positive-interval
                       nonpositive-interval))

(define-values (nonpositive?/bot nonpositive?/pre)
  (real-predicate/prim 'nonpositive?
                       (λ: ([x : Flonum]) (x . fl<= . 0.0))
                       nonpositive-interval
                       positive-interval))

(define-values (nonnegative?/bot nonnegative?/pre)
  (real-predicate/prim 'nonnegative?
                       (λ: ([x : Flonum]) (x . fl>= . 0.0))
                       nonnegative-interval
                       negative-interval))

(define </bot (>>>/bot -/bot negative?/bot))
(define >/bot (>>>/bot -/bot positive?/bot))
(define <=/bot (>>>/bot -/bot nonpositive?/bot))
(define >=/bot (>>>/bot -/bot nonnegative?/bot))

(define </pre (>>>/pre -/pre negative?/pre))
(define >/pre (>>>/pre -/pre positive?/pre))
(define <=/pre (>>>/pre -/pre nonpositive?/pre))
(define >=/pre (>>>/pre -/pre nonnegative?/pre))

;; ---------------------------------------------------------------------------------------------------
;; Nonmonotone functions

(define nonzero-reals
  (real-set-union (Nonextremal-Interval -inf.0 0.0 #f #f)
                  (Nonextremal-Interval 0.0 +inf.0 #f #f)))

;; Absolute value
(define abs/bot (real/bot 'abs reals nonnegative-interval flabs))
(define abs/pre (ifte/pre negative?/pre neg/pre id/pre))

;; Square
(define sqr/bot (real/bot 'sqr reals nonnegative-interval flsqr))
(define sqr/pre (ifte/pre negative?/pre neg-sqr/pre pos-sqr/pre))

;; Reciprocal
(define recip/bot (real/bot 'recip nonzero-reals nonzero-reals flrecip))
(define recip/pre
  (ifte/pre positive?/pre pos-recip/pre (ifte/pre negative?/pre neg-recip/pre fail/pre)))

;; Multiplication

(define mul-domain (set-pair reals reals))
(define */bot (real2d/bot '* reals reals reals fl*))
(define */pre
  (ifte/pre (>>>/pre (ref/pre 'fst) positive?/pre)
            (ifte/pre (>>>/pre (ref/pre 'snd) positive?/pre)
                      pos-pos-mul/pre
                      (ifte/pre (>>>/pre (ref/pre 'snd) negative?/pre)
                                pos-neg-mul/pre
                                ((restrict/pre mul-domain) . >>>/pre . (const/pre 0.0))))
            (ifte/pre (>>>/pre (ref/pre 'fst) negative?/pre)
                      (ifte/pre (>>>/pre (ref/pre 'snd) positive?/pre)
                                neg-pos-mul/pre
                                (ifte/pre (>>>/pre (ref/pre 'snd) negative?/pre)
                                          neg-neg-mul/pre
                                          ((restrict/pre mul-domain) . >>>/pre . (const/pre 0.0))))
                      ((restrict/pre mul-domain) . >>>/pre . (const/pre 0.0)))))

;; Division

(define div-domain (set-pair reals nonzero-reals))
(define //bot (real2d/bot '/ reals nonzero-reals reals fl/))
(define //pre
  (ifte/pre (>>>/pre (ref/pre 'snd) positive?/pre)
            (ifte/pre (>>>/pre (ref/pre 'fst) positive?/pre)
                      pos-pos-div/pre
                      (ifte/pre (>>>/pre (ref/pre 'fst) negative?/pre)
                                neg-pos-div/pre
                                ((restrict/pre div-domain) . >>>/pre . (const/pre 0.0))))
            (ifte/pre (>>>/pre (ref/pre 'snd) negative?/pre)
                      (ifte/pre (>>>/pre (ref/pre 'fst) positive?/pre)
                                pos-neg-div/pre
                                (ifte/pre (>>>/pre (ref/pre 'fst) negative?/pre)
                                          neg-neg-div/pre
                                          ((restrict/pre div-domain) . >>>/pre . (const/pre 0.0))))
                      fail/pre)))

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
  (ifte negative? (>>> neg mono-cos) mono-cos))

;; Sine restricted to [-π/2,π]
(define-syntax-rule (make-partial-pos-sin >>> ifte mono-sin translate nonpositive? neg fail)
  (ifte (>>> (translate (* -0.5 pi)) nonpositive?)
        mono-sin
        (>>> (translate (- pi))
             (ifte nonpositive?
                   (>>> neg mono-sin)
                   fail))))

;; Sine restricted to [-π,π]
(define-syntax-rule (make-partial-sin >>> ifte partial-pos-sin negative? neg)
  (ifte negative?
        (>>> (>>> neg partial-pos-sin) neg)
        partial-pos-sin))

(define partial-cos/bot (make-partial-cos >>>/bot ifte/bot mono-cos/bot negative?/bot neg/bot))
(define partial-cos/pre (make-partial-cos >>>/pre ifte/pre mono-cos/pre negative?/pre neg/pre))

(define partial-pos-sin/bot
  (make-partial-pos-sin >>>/bot ifte/bot
                        mono-sin/bot translate/bot nonpositive?/bot neg/bot fail/bot))

(define partial-pos-sin/pre
  (make-partial-pos-sin >>>/pre ifte/pre
                        mono-sin/pre translate/pre nonpositive?/pre neg/pre fail/pre))

(define partial-sin/bot (make-partial-sin >>>/bot ifte/bot partial-pos-sin/bot negative?/bot neg/bot))
(define partial-sin/pre (make-partial-sin >>>/pre ifte/pre partial-pos-sin/pre negative?/pre neg/pre))
