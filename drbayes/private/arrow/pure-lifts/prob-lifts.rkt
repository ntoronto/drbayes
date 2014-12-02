#lang typed/racket/base

(require math/distributions
         "../../set.rkt"
         "../../flonum.rkt"
         "make-prob-lift.rkt")

(provide normal-inv-cdf/bot normal-inv-cdf/pre
         cauchy-inv-cdf/bot cauchy-inv-cdf/pre
         uniform-inv-cdf/bot uniform-inv-cdf/pre)

(: flnormal-inv-cdf/near (-> Flonum Flonum))
(define (flnormal-inv-cdf/near p)
  (if (< p 0.0)
      (flnormal-inv-cdf 0.0 1.0 p #t #f)
      (flnormal-inv-cdf 0.0 1.0 (- p) #t #t)))

(: flnormal-cdf/near (-> Flonum Flonum))
(define (flnormal-cdf/near x)
  (if (< x 0.0)
      (flnormal-cdf 0.0 1.0 x #t #f)
      (- (flnormal-cdf 0.0 1.0 x #t #t))))

(: flcauchy-inv-cdf/near (-> Flonum Flonum))
(define (flcauchy-inv-cdf/near p)
  (if (< p 0.0)
      (flcauchy-inv-cdf 0.0 1.0 p #t #f)
      (flcauchy-inv-cdf 0.0 1.0 (- p) #t #t)))

(: flcauchy-cdf/near (-> Flonum Flonum))
(define (flcauchy-cdf/near x)
  (if (< x 0.0)
      (flcauchy-cdf 0.0 1.0 x #t #f)
      (- (flcauchy-cdf 0.0 1.0 x #t #t))))

(define fluniform-inv-cdf/near flprob->flonum)
(define fluniform-inv-cdf/rndd flprob->flonum/rndd)
(define fluniform-inv-cdf/rndu flprob->flonum/rndu)

(: fluniform-cdf/near (-> Flonum Flonum))
(: fluniform-cdf/rndd (-> Flonum Flonum))
(: fluniform-cdf/rndu (-> Flonum Flonum))

(define (fluniform-cdf/near p) (if (< p 0.0) 0.0 (flonum->flprob p)))
(define (fluniform-cdf/rndd p) (if (< p 0.0) 0.0 (flonum->flprob/rndd p)))
(define (fluniform-cdf/rndu p) (if (< p 0.0) 0.0 (flonum->flprob/rndu p)))

(define normal-inv-cdf/bot (inverse-cdf/bot 'normal reals flnormal-inv-cdf/near))
(define normal-inv-cdf/pre
  (make-pre-arrow (inverse-cdf-img/fake-rnd reals flnormal-inv-cdf/near 4)
                  (inverse-cdf-pre/fake-rnd reals flnormal-cdf/near 4)))

(define cauchy-inv-cdf/bot (inverse-cdf/bot 'cauchy reals flcauchy-inv-cdf/near))
(define cauchy-inv-cdf/pre
  (make-pre-arrow (inverse-cdf-img/fake-rnd reals flcauchy-inv-cdf/near 4)
                  (inverse-cdf-pre/fake-rnd reals flcauchy-cdf/near 4)))

(define uniform-inv-cdf/bot (inverse-cdf/bot 'uniform reals fluniform-inv-cdf/near))
(define uniform-inv-cdf/pre
  (make-pre-arrow (inverse-cdf-img reals fluniform-inv-cdf/rndd fluniform-inv-cdf/rndu)
                  (inverse-cdf-pre reals fluniform-cdf/rndd fluniform-cdf/rndu)))
