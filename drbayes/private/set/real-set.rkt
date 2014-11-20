#lang typed/racket/base

(require racket/match
         math/flonum
         "ordered-set.rkt"
         "../utils.rkt")

(provide (all-defined-out))

(define-ordered-set
  #:names Real real
  #:types Flonum
  #:predicates
  (λ (x) (<= -inf.0 x +inf.0))
  (λ (x) (< -inf.0 x +inf.0))
  (λ (x) (= x -inf.0))
  (λ (x) (= x +inf.0))
  #:comparisons = <
  #:guards
  (λ (a b a? b?) (or (= a +inf.0) (= b -inf.0)))
  (λ (a b a? b?) (and (= a -inf.0) (= b +inf.0)))
  (λ (a b a? b?)
    (values (if (= a 0.0) +0.0 a)
            (if (= b 0.0) -0.0 b)
            (if (= a -inf.0) #f a?)
            (if (= b +inf.0) #f b?)))
  )

(define positive-interval (Plain-Real-Interval 0.0 +inf.0 #f #f))
(define negative-interval (Plain-Real-Interval -inf.0 -0.0 #f #f))
(define nonnegative-interval (Plain-Real-Interval 0.0 +inf.0 #t #f))
(define nonpositive-interval (Plain-Real-Interval -inf.0 -0.0 #f #t))
(define zero-interval (Plain-Real-Interval +0.0 -0.0 #t #t))
(define unit-interval (Plain-Real-Interval 0.0 1.0 #t #t))

(: flonum->singleton (-> Flonum Plain-Real-Set))
(define (flonum->singleton x)
  (cond [(< -inf.0 x +inf.0)  (Plain-Real-Interval x x #t #t)]
        [else  (raise-argument-error 'flonum->singleton "rational Flonum" x)]))

(: real-interval-fields (-> Nonempty-Real-Interval (Values Flonum Flonum Boolean Boolean)))
(define (real-interval-fields I)
  (if (reals? I)
      (values -inf.0 +inf.0 #f #f)
      (values (Plain-Real-Interval-min I)
              (Plain-Real-Interval-max I)
              (Plain-Real-Interval-min? I)
              (Plain-Real-Interval-max? I))))

(: real-interval-sample-point (-> Plain-Real-Interval Flonum))
(define (real-interval-sample-point I)
  (match-define (Plain-Real-Interval a b a? b?) I)
  (define m (- b a))
  (define x (+ a (* m (random))))
  (cond [(and (or (not (= x a)) a?) (or (not (= x b)) b?))  x]
        [(and a? b?)  (* 0.5 (+ a b))]
        [a?  a]
        [b?  b]
        [else  (* 0.5 (+ a b))]))

(: real-interval-measure (-> Real-Interval Flonum))
(define (real-interval-measure I)
  (cond [(empty-real-set? I)  0.0]
        [(reals? I)   +inf.0]
        [else  (- (Plain-Real-Interval-max I) (Plain-Real-Interval-min I))]))

(: real-set-sample-point (-> Plain-Real-Set Flonum))
(define (real-set-sample-point I)
  (cond [(Plain-Real-Interval? I)  (real-interval-sample-point I)]
        [else
         (define Is (Plain-Real-Interval-List-elements I))
         (define i (sample-index (normalize-probs/+2 (map/+2 real-interval-measure Is))))
         (real-interval-sample-point (list-ref Is i))]))

(: real-set-measure (-> Real-Set Flonum))
(define (real-set-measure I)
  (cond [(empty-real-set? I)  0.0]
        [(reals? I)  +inf.0]
        [(Plain-Real-Interval? I)   (real-interval-measure I)]
        [else  (flsum (map real-interval-measure (Plain-Real-Interval-List-elements I)))]))
