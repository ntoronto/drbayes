#lang typed/racket/base

(require racket/performance-hint
         racket/fixnum
         math/flonum
         math/distributions)

(provide (all-defined-out))

(begin-encourage-inline
  
  (: flneg   (-> Flonum Flonum))
  (: flsqr   (-> Flonum Flonum))
  (: flrecip (-> Flonum Flonum))
  
  (define (flneg x)   (fl* -1.0 x))
  (define (flsqr x)   (fl* x x))
  (define (flrecip x) (fl/ 1.0 x))
  
  (: flscale     (-> Flonum (-> Flonum Flonum)))
  (: fltranslate (-> Flonum (-> Flonum Flonum)))
  
  (define ((flscale y) x) (* x y))
  (define ((fltranslate y) x) (+ x y))
  
  (: flcauchy (-> Flonum Flonum))
  (: flnormal (-> Flonum Flonum))
  (: flcauchy-inv (-> Flonum Flonum))
  (: flnormal-inv (-> Flonum Flonum))
  
  (define (flcauchy p) (flcauchy-inv-cdf 0.0 1.0 p #f #f))
  (define (flnormal p) (flnormal-inv-cdf 0.0 1.0 p #f #f))
  (define (flcauchy-inv x) (flcauchy-cdf 0.0 1.0 x #f #f))
  (define (flnormal-inv x) (flnormal-cdf 0.0 1.0 x #f #f))
  
  (: flrecip/error (-> Flonum (Values Flonum Flonum)))
  (: fllog/error   (-> Flonum (Values Flonum Flonum)))
  (: fllog1p/error (-> Flonum (Values Flonum Flonum)))
  
  (define (flrecip/error x) (fl//error 1.0 x))
  (define (fllog/error x)   (fl2log x 0.0))
  (define (fllog1p/error x) (fl2log1p x 0.0))
  
  )  ; begin-encourage-inline

(: flstep* (-> Flonum Fixnum Flonum))
;; This could definitely be faster
(define (flstep* x n)
  (cond [(fx> n 0)
         (let loop ([x  (flnext* x)] [n  (- n 1)])
           (cond [(fx= n 0)  x]
                 [else  (loop (flnext* x) (- n 1))]))]
        [(fx< n 0)
         (let loop ([x  (flprev* x)] [n  (+ n 1)])
           (cond [(fx= n 0)  x]
                 [else  (loop (flprev* x) (+ n 1))]))]
        [else  x]))

(: fllog-random (-> Flonum Flonum Flonum))
(define (fllog-random a b)
  (let ([a  (min a b)]
        [b  (max a b)])
    (let loop ()
      (define x (lg+ a (+ (fllog (random)) (lg- b a))))
      (if (<= a x b) x (loop)))))
