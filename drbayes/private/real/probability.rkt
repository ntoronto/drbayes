#lang typed/racket/base

(require racket/performance-hint
         math/flonum
         racket/promise
         "../flonum.rkt"
         "../set/bottom.rkt")

(provide (rename-out [-Probability  Probability])
         prob-0
         prob-1
         prob->flonum prob->flonum/rndd prob->flonum/rndu
         flonum->prob
         prob1-
         prob* prob*/rndd prob*/rndu
         prob/ prob//rndd prob//rndu
         prob+ prob+/rndd prob+/rndu
         prob- prob-/rndd prob-/rndu
         prob-midpoint
         prob-random
         prob= prob< prob<= prob> prob>=)

(struct Probability ([value : Flonum]) #:transparent)

(define-type -Probability Probability)

(define prob-0 (Probability -inf.0))
(define prob-1 (Probability +inf.0))

(: prob->flonum (-> Probability Flonum))
(: prob->flonum/rndd (-> Probability Flonum))
(: prob->flonum/rndu (-> Probability Flonum))

(define (prob->flonum x) (flprob->flonum (Probability-value x)))
(define (prob->flonum/rndd x) (flprob->flonum/rndd (Probability-value x)))
(define (prob->flonum/rndu x) (flprob->flonum/rndu (Probability-value x)))

(: flonum->prob (-> Flonum (U Bottom Probability)))
(define (flonum->prob p)
  (define x (flonum->flprob p))
  (if (flnan? x)
      (bottom (delay (format "flonum->prob: not a probability: ~e" p)))
      (Probability x)))

;(: make-prob-fun (-> Symbol (-> Flonum Flonum) (-> prob (U Bottom prob))))
(define-syntax-rule (make-prob-fun name f)
  (λ ([x : Probability])
    (define y (f (Probability-value x)))
    (if (flnan? y)
        (bottom (delay (format "(~a ~e) is not a probability" name x)))
        (Probability y))))

;(: make-prob-fun/total (-> Symbol (-> Flonum Flonum) (-> prob prob)))
(define-syntax-rule (make-prob-fun/total name f)
  (λ ([x : Probability])
    (define y (f (Probability-value x)))
    (if (flnan? y)
        (error name "(~a ~e) is not a probability" name x)
        (Probability y))))

;(: make-prob-2d-fun (-> Symbol (-> Flonum Flonum Flonum) (-> prob prob (U Bottom prob))))
(define-syntax-rule (make-prob-2d-fun name f)
  (λ ([x : Probability] [y : Probability])
    (define z (f (Probability-value x) (Probability-value y)))
    (if (flnan? z)
        (bottom (delay (format "(~a ~e ~e) is not a probability" name x y)))
        (Probability z))))

;(: make-prob-2d-fun/total (-> Symbol (-> Flonum Flonum Flonum) (-> prob prob prob)))
(define-syntax-rule (make-prob-2d-fun/total name f)
  (λ ([x : Probability] [y : Probability])
    (define z (f (Probability-value x) (Probability-value y)))
    (if (flnan? z)
        (error name "(~a ~e ~e) is not a probability" name x y)
        (Probability z))))

;(: make-prob-comp-fun (-> (-> Flonum Flonum Boolean) (-> prob prob Boolean)))
(define-syntax-rule (make-prob-comp-fun f)
  (λ ([x : Probability] [y : Probability])
    (f (Probability-value x) (Probability-value y))))

(define prob1- (make-prob-fun/total 'prob1- flprob1-))

(define prob* (make-prob-2d-fun/total 'prob* flprob*))
(define prob*/rndd (make-prob-2d-fun/total 'prob*/rndd flprob*/rndd))
(define prob*/rndu (make-prob-2d-fun/total 'prob*/rndu flprob*/rndu))

(define prob/ (make-prob-2d-fun 'prob/ flprob/))
(define prob//rndd (make-prob-2d-fun 'prob//rndd flprob//rndd))
(define prob//rndu (make-prob-2d-fun 'prob//rndu flprob//rndu))

(define prob+ (make-prob-2d-fun 'prob+ flprob+))
(define prob+/rndd (make-prob-2d-fun 'prob+/rndd flprob+/rndd))
(define prob+/rndu (make-prob-2d-fun 'prob+/rndu flprob+/rndu))

(define prob- (make-prob-2d-fun 'prob- flprob-))
(define prob-/rndd (make-prob-2d-fun 'prob-/rndd flprob-/rndd))
(define prob-/rndu (make-prob-2d-fun 'prob-/rndu flprob-/rndu))

(define prob-midpoint (make-prob-2d-fun/total 'prob-midpoint flprob-midpoint))
(define prob-random (make-prob-2d-fun/total 'prob-random flprob-random))

(define prob=  (make-prob-comp-fun flprob=))
(define prob<  (make-prob-comp-fun flprob<))
(define prob<= (make-prob-comp-fun flprob<=))
(define prob>  (make-prob-comp-fun flprob>))
(define prob>= (make-prob-comp-fun flprob>=))
