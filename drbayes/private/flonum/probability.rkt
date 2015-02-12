#lang typed/racket/base

(require racket/performance-hint
         racket/list
         math/flonum
         racket/promise
         "symmetric-log.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide Bad-Prob bad-prob bad-prob?
         (struct-out Prob)
         prob?
         prob-0 prob-0?
         prob-1 prob-1?
         prob->flonum prob->flonum/rndd prob->flonum/rndu
         flonum->prob flonum->prob/rndd flonum->prob/rndu
         prob1-
         prob* prob*/rndd prob*/rndu
         prob/ prob//rndd prob//rndu
         prob+ prob+/rndd prob+/rndu
         prob- prob-/rndd prob-/rndu
         prob-midpoint
         prob-boolean
         prob-random
         prob= prob< prob<= prob> prob>=
         prob-min prob-max
         prob-random-index
         prob-sum
         prob-normalize
         prob-normalize/+2
         prob-quotient->flonum)

(define-singleton-type Bad-Prob bad-prob)

(struct Prob ([value : Flonum]) #:transparent)

(define prob? Prob?)

(define prob-0 (Prob -inf.0))
(define prob-1 (Prob +inf.0))

(define prob-0? (λ ([x : Prob]) (= (Prob-value x) -inf.0)))
(define prob-1? (λ ([x : Prob]) (= (Prob-value x) +inf.0)))

(define prob->flonum (λ ([x : Prob]) (flprob->flonum (Prob-value x))))
(define prob->flonum/rndd (λ ([x : Prob]) (flprob->flonum/rndd (Prob-value x))))
(define prob->flonum/rndu (λ ([x : Prob]) (flprob->flonum/rndu (Prob-value x))))

(: make-flonum->prob (-> (-> Flonum Flonum) (-> Flonum (U Bad-Prob Prob))))
(define ((make-flonum->prob flonum->flprob) p)
  (define x (flonum->flprob p))
  (if (flnan? x) bad-prob (Prob x)))

(define flonum->prob (make-flonum->prob flonum->flprob))
(define flonum->prob/rndd (make-flonum->prob flonum->flprob/rndd))
(define flonum->prob/rndu (make-flonum->prob flonum->flprob/rndu))

(: make-prob-2d-fun (-> (-> Flonum Flonum Flonum) (-> Prob Prob (U Bad-Prob Prob))))
(define (make-prob-2d-fun f)
  (λ ([x : Prob] [y : Prob])
    (define z (f (Prob-value x) (Prob-value y)))
    (if (flnan? z) bad-prob (Prob z))))

(: make-prob-2d-fun/total (-> Symbol (-> Flonum Flonum Flonum) (-> Prob Prob Prob)))
(define (make-prob-2d-fun/total name f)
  (λ ([x : Prob] [y : Prob])
    (define z (f (Prob-value x) (Prob-value y)))
    (if (flnan? z)
        (error name "internal error: (~a ~e ~e) is not a probability" name x y)
        (Prob z))))

(: make-prob-comp-fun (-> (-> Flonum Flonum Boolean) (-> Prob Prob Boolean)))
(define (make-prob-comp-fun f)
  (λ ([x : Prob] [y : Prob])
    (f (Prob-value x) (Prob-value y))))

(: prob1- (-> Prob Prob))
(define (prob1- x) (Prob (flprob1- (Prob-value x))))

(define prob* (make-prob-2d-fun/total 'prob* flprob*))
(define prob*/rndd (make-prob-2d-fun/total 'prob*/rndd flprob*/rndd))
(define prob*/rndu (make-prob-2d-fun/total 'prob*/rndu flprob*/rndu))

(define prob/ (make-prob-2d-fun flprob/))
(define prob//rndd (make-prob-2d-fun flprob//rndd))
(define prob//rndu (make-prob-2d-fun flprob//rndu))

(define prob+ (make-prob-2d-fun flprob+))
(define prob+/rndd (make-prob-2d-fun flprob+/rndd))
(define prob+/rndu (make-prob-2d-fun flprob+/rndu))

(define prob- (make-prob-2d-fun flprob-))
(define prob-/rndd (make-prob-2d-fun flprob-/rndd))
(define prob-/rndu (make-prob-2d-fun flprob-/rndu))

(define prob-midpoint (make-prob-2d-fun/total 'prob-midpoint flprob-midpoint))
(define prob-boolean (λ ([p : Prob]) (flprob-boolean (Prob-value p))))
(define prob-random (make-prob-2d-fun/total 'prob-random flprob-random))

(define prob=  (make-prob-comp-fun flprob=))
(define prob<  (make-prob-comp-fun flprob<))
(define prob<= (make-prob-comp-fun flprob<=))
(define prob>  (make-prob-comp-fun flprob>))
(define prob>= (make-prob-comp-fun flprob>=))

(: prob-min (-> Prob Prob Prob))
(: prob-max (-> Prob Prob Prob))

(define (prob-min x y) (if (prob< x y) x y))
(define (prob-max x y) (if (prob< x y) y x))

(: prob-random-index (-> (Listof Prob) Index))
(define (prob-random-index ps)
  (cond [(empty? ps)  (raise-argument-error 'prob-random-index "nonempty list" ps)]
        [else
         (define r (prob-random prob-0 prob-1))
         (let loop ([i : Index  0] [p  (first ps)] [ps  (rest ps)])
           (cond [(or (empty? ps) (prob< r p))  i]
                 [else
                  (let ([p  (let ([p  (prob+ p (first ps))])
                              (if (bad-prob? p) prob-1 p))])
                    (loop (assert (+ i 1) index?) p (rest ps)))]))]))

(: prob-sum (-> (Listof Prob) (U Bad-Prob Prob)))
(define (prob-sum qs)
  (let loop ([qs qs] [p prob-0])
    (cond [(empty? qs)  p]
          [else  (let ([p  (prob+ p (first qs))])
                   (if (prob? p) (loop (rest qs) p) p))])))

(: prob-normalize (-> (Listof Prob) (Listof (U Bad-Prob Prob))))
(define (prob-normalize qs)
  (define p (prob-sum qs))
  (cond [(not (prob? p))
         (define x (assert (/ 0.5 (apply + (map prob->flonum qs))) (λ ([x : Flonum]) (>= x 0.0))))
         (define p (assert (flonum->prob x) prob?))
         (prob-normalize (map (λ ([q : Prob]) (prob* q p)) qs))]
        [(prob-0? p)
         (make-list (length qs) bad-prob)]
        [else
         (map (λ ([q : Prob])
                (let ([q  (prob/ q p)])
                  (if (prob? q) q prob-1)))
              qs)]))

(: prob-normalize/+2 (-> (Listof+2 Prob) (U #f (Listof+2 Prob))))
(define (prob-normalize/+2 qs)
  (define p (prob-sum qs))
  (cond [(not (prob? p))
         (define x (assert (/ 0.5 (apply + (map prob->flonum qs))) (λ ([x : Flonum]) (>= x 0.0))))
         (define p (assert (flonum->prob x) prob?))
         (prob-normalize/+2 (map/+2 (λ ([q : Prob]) (prob* q p)) qs))]
        [(prob-0? p)  #f]
        [else
         (map/+2 (λ ([q : Prob])
                   (let ([q  (prob/ q p)])
                     (if (prob? q) q prob-1)))
                 qs)]))

(: prob-quotient->flonum (-> Prob Prob Nonnegative-Flonum))
(define (prob-quotient->flonum numer denom)
  (define mx (prob-max numer denom))
  (let ([numer  (assert (prob/ numer mx) prob?)]
        [denom  (assert (prob/ denom mx) prob?)])
    (abs (/ (prob->flonum numer)
            (prob->flonum denom)))))
