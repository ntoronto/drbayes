#lang typed/racket/base

(require racket/match
         racket/list
         math/flonum
         "ordered-set.rkt"
         "../flonum.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(define-ordered-set
  #:names Prob prob
  #:types Prob
  #:predicates
  (λ (x) #t)
  (λ (x) #t)
  prob-0?
  prob-1?
  #:comparisons prob= prob<
  #:guards
  (λ (a b a? b?) #f)
  (λ (a b a? b?) #f)
  (λ (a b a? b?) (values a b a? b?))
  )

;; ===================================================================================================
;; More Prob-Interval ops

(: prob->singleton (-> Prob Plain-Prob-Set))
(define (prob->singleton x)
  (plain-prob-interval x x #t #t))

(: prob-interval-fields (-> Nonempty-Prob-Interval (Values Prob Prob Boolean Boolean)))
(define (prob-interval-fields I)
  (if (probs? I)
      (values prob-0 prob-1 #t #t)
      (values (Plain-Prob-Interval-min I)
              (Plain-Prob-Interval-max I)
              (Plain-Prob-Interval-min? I)
              (Plain-Prob-Interval-max? I))))

(: make-prob-interval-measure (-> (-> Prob Prob (U Prob Bad-Prob)) (-> Prob-Interval Prob)))
(define ((make-prob-interval-measure prob-) I)
  (cond [(empty-prob-set? I)  prob-0]
        [(probs? I)   prob-1]
        [else
         (define p (prob- (Plain-Prob-Interval-max I) (Plain-Prob-Interval-min I)))
         (cond [(prob? p)  p]
               [else  (error 'prob-interval-measure "result is not a probability; given ~e" I)])]))

(define prob-interval-measure (make-prob-interval-measure prob-))
(define prob-interval-measure/rndd (make-prob-interval-measure prob-/rndd))
(define prob-interval-measure/rndu (make-prob-interval-measure prob-/rndu))

(: prob-next (-> Prob Prob))
(define (prob-next x)
  (Prob (flprob-fast-canonicalize (flnext* (Prob-value x)))))

(: prob-prev (-> Prob Prob))
(define (prob-prev x)
  (Prob (flprob-fast-canonicalize (flprev* (Prob-value x)))))

(: prob-interval-can-sample? (-> Nonempty-Prob-Interval Boolean))
(define (prob-interval-can-sample? I)
  (define-values (a b a? b?) (prob-interval-fields I))
  (not (and (not a?) (not b?) (prob= (prob-next a) b))))

(: prob-interval-sample-point (-> Nonempty-Prob-Interval (U Bad-Prob Prob)))
(define (prob-interval-sample-point I)
  (define-values (a b a? b?) (prob-interval-fields I))
  (let loop ()
    (define x (prob-random a b))
    (cond [(not (or (and (not a?) (prob= x a)) (and (not b?) (prob= x b))))  x]
          [(prob-interval-can-sample? I)  (loop)]
          [else  bad-prob])))

;; ===================================================================================================
;; More ops

(: make-prob-interval-list-measure (-> (-> Nonempty-Prob-Interval Prob)
                                       (-> Prob Prob (U Prob Bad-Prob))
                                       (-> (Listof+2 Nonempty-Prob-Interval) Prob)))
(define ((make-prob-interval-list-measure prob-interval-measure prob+) Is)
  (for/fold ([q : Prob  prob-0]) ([I  (in-list Is)])
    (let ([q  (prob+ q (prob-interval-measure I))])
      (if (prob? q) q prob-1))))

(define prob-interval-list-measure
  (make-prob-interval-list-measure prob-interval-measure prob+))

(define prob-interval-list-measure/rndd
  (make-prob-interval-list-measure prob-interval-measure/rndd prob+/rndd))

(define prob-interval-list-measure/rndu
  (make-prob-interval-list-measure prob-interval-measure/rndu prob+/rndu))

(: prob-interval-list-probs (-> (Listof+2 Nonempty-Prob-Interval) (U #f (Listof+2 Prob))))
(define (prob-interval-list-probs Is)
  (define q (prob-interval-list-measure Is))
  (cond [(prob-0? q)  #f]
        [else
         (map/+2 (λ ([I : Nonempty-Prob-Interval])
                   (define p (prob/ (prob-interval-measure I) q))
                   (if (prob? p) p prob-1))
                 Is)]))

(: make-prob-set-measure (-> (-> Plain-Prob-Interval Prob)
                             (-> (Listof+2 Plain-Prob-Interval) Prob)
                             (-> Prob-Set Prob)))
(define ((make-prob-set-measure prob-interval-measure prob-interval-list-measure) I)
  (cond [(empty-prob-set? I)  prob-0]
        [(probs? I)  prob-1]
        [(Plain-Prob-Interval? I)   (prob-interval-measure I)]
        [else  (prob-interval-list-measure (Prob-Interval-List-elements I))]))

(define prob-set-measure
  (make-prob-set-measure prob-interval-measure prob-interval-list-measure))

(define prob-set-measure/rndd
  (make-prob-set-measure prob-interval-measure/rndd prob-interval-list-measure/rndd))

(define prob-set-measure/rndu
  (make-prob-set-measure prob-interval-measure/rndu prob-interval-list-measure/rndu))

(: prob-set-sample-point (-> Nonempty-Prob-Set (U Bad-Prob Prob)))
(define (prob-set-sample-point I)
  (cond [(or (probs? I) (Plain-Prob-Interval? I))  (prob-interval-sample-point I)]
        [else
         (define q (prob-set-measure I))
         (define Is (filter prob-interval-can-sample? (Prob-Interval-List-elements I)))
         (cond [(empty? Is)  bad-prob]
               [(empty? (rest Is))  (prob-interval-sample-point (first Is))]
               [else
                (define ps (prob-interval-list-probs Is))
                (cond [ps  (define i (prob-random-index ps))
                           (prob-interval-sample-point (list-ref Is i))]
                      [else  bad-prob])])]))

(: prob-set-self-join (case-> (-> Nonempty-Prob-Set (Values Nonempty-Prob-Interval Boolean))
                              (-> Prob-Set (Values Prob-Interval Boolean))))
(define (prob-set-self-join I)
  (cond [(empty-prob-set? I)  (values empty-prob-set #t)]
        [(or (probs? I) (Plain-Prob-Interval? I))  (values I #t)]
        [else
         (define Is (Prob-Interval-List-elements I))
         (define-values (I1 exact?) (prob-interval-join (first Is) (second Is)))
         (for/fold ([I1 : Nonempty-Prob-Interval  I1]
                    [exact? : Boolean  exact?])
                   ([I2  (in-list (rest (rest Is)))])
           (let-values ([(I1 e?)  (prob-interval-join I1 I2)])
             (values I1 (and e? exact?))))]))
