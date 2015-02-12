#lang typed/racket/base

(require math/flonum
         "../../set.rkt"
         "../../flonum.rkt"
         "make-real-lift.rkt")

(provide floor/bot ceiling/bot round/bot truncate/bot
         floor/pre ceiling/pre round/pre truncate/pre)

(: monotone-interval-img (-> (-> Flonum Flonum) (-> Nonempty-Real-Interval
                                                    (Values Real-Interval Boolean))))
(define ((monotone-interval-img f) A)
  (define-values (a1 a2 a1? a2?) (real-interval-fields A))
  (values (real-interval (f a1) (f a2) #t #t)
          #f))

(define flfloor-img (monotone-interval-img flfloor))
(define flceiling-img (monotone-interval-img flceiling))
(define flround-img (monotone-interval-img flround))
(define fltruncate-img (monotone-interval-img fltruncate))

(: flfloor-pre (-> Nonempty-Real-Interval (Values Real-Interval Boolean)))
(define (flfloor-pre B)
  (define-values (b1 b2 b1? b2?) (real-interval-fields B))
  (define A
    (real-interval (if b1? (flceiling b1) (fl+/rndd (flfloor b1) 1.0))
                   (if b2? (fl+/rndu (flfloor b2) 1.0) (flceiling b2))
                   #t
                   #f))
  (values A #t))

(: flceiling-pre (-> Nonempty-Real-Interval (Values Real-Interval Boolean)))
(define (flceiling-pre B)
  (define-values (b1 b2 b1? b2?) (real-interval-fields B))
  (define A
    (real-interval (if b1? (fl-/rndd (flceiling b1) 1.0) (flfloor b1))
                   (if b2? (flfloor b2) (fl-/rndu (flceiling b2) 1.0))
                   #f
                   #t))
  (values A #t))

(: flround-pre (-> Nonempty-Real-Interval (Values Real-Interval Boolean)))
(define (flround-pre B)
  (define-values (b1 b2 b1? b2?) (real-interval-fields B))
  (define A
    (real-interval (if b1? (fl-/rndd (flceiling b1) 0.5) (fl+/rndd (flfloor b1) 0.5))
                   (if b2? (fl+/rndu (flfloor b2) 0.5) (fl-/rndu (flceiling b2) 0.5))
                   (if b1? (fleven? (flceiling b1)) (flodd? (flfloor b1)))
                   (if b2? (fleven? (flfloor b2)) (flodd? (flceiling b2)))))
  (values A #t))

(: fltruncate-pre (-> Nonempty-Real-Interval (Values Real-Interval Boolean)))
(define (fltruncate-pre B)
  (define-values (b1 b2 b1? b2?) (real-interval-fields B))
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
  (define A (real-interval a1 a2 a1? a2?))
  (values A #t))

(define-values (floor/bot floor/pre)
  (monotone/prim 'floor reals reals flfloor flfloor-img flfloor-pre))

(define-values (ceiling/bot ceiling/pre)
  (monotone/prim 'ceiling reals reals flceiling flceiling-img flceiling-pre))

(define-values (round/bot round/pre)
  (monotone/prim 'round reals reals flround flround-img flround-pre))

(define-values (truncate/bot truncate/pre)
  (monotone/prim 'truncate reals reals fltruncate fltruncate-img fltruncate-pre))
