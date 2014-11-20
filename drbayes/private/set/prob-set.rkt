#lang typed/racket/base

(require racket/match
         math/flonum
         "real-set.rkt"
         "../utils.rkt"
         "../flonum.rkt")

(provide probs
         prob-set-sample-point
         prob-set-measure)

(define probs (Plain-Interval-List
               (list
                (Plain-Interval -inf.0 (fllog 0.5) #t #t)
                (Plain-Interval (fllog 2.0) +inf.0 #f #t))))

(: prob-interval-fields (-> Nonempty-Interval (Values Flonum Flonum Boolean Boolean)))
(define (prob-interval-fields I)
  (cond [(extended-reals? I)  (values -inf.0 +inf.0 #t #t)]
        [else
         (define a (Plain-Interval-min I))
         (define b (Plain-Interval-max I))
         (values (if (= a (fllog 2.0)) (fllog 0.5) a)
                 (if (= b (fllog 2.0)) (fllog 0.5) b)
                 (Plain-Interval-min? I)
                 (Plain-Interval-max? I))]))

(: prob-interval-sample-point (-> Plain-Interval Flonum))
(define (prob-interval-sample-point I)
  (define-values (a b a? b?) (prob-interval-fields I))
  (let loop ()
    (define x (flprob-random a b))
    (if (and (or (not (= x a)) a?)
             (or (not (= x b)) b?))
        x
        (loop))))

(: prob-interval-measure (Interval -> Flonum))
(define (prob-interval-measure I)
  (cond [(empty-real-set? I)  -inf.0]
        [(extended-reals? I)   +inf.0]
        [else  (define-values (a b _a? _b?) (prob-interval-fields I))
               (flprob- b a)]))

(: prob-set-sample-point (-> Real-Set Flonum))
(define (prob-set-sample-point orig-I)
  (define I (real-set-intersect orig-I probs))
  (cond [(empty-real-set? I)
         (raise-argument-error 'prob-set-sample-point "nonempty probability set" orig-I)]
        [(Plain-Interval? I)
         (prob-interval-sample-point I)]
        [else
         (define Is (Plain-Interval-List-elements I))
         (define p (prob-set-measure I))
         (define i (sample-index (map/+2 (λ ([I : Plain-Interval])
                                           (flprob->flonum
                                            (flprob/ (prob-interval-measure I) p)))
                                         Is)))
         (prob-interval-sample-point (list-ref Is i))]))

(: prob-set-measure (-> Real-Set Flonum))
(define (prob-set-measure I)
  (let ([I  (real-set-intersect I probs)])
    (cond [(empty-real-set? I)  -inf.0]
          [(Plain-Interval? I)   (prob-interval-measure I)]
          [else
           (for/fold ([p : Flonum  -inf.0]) ([I  (in-list (Plain-Interval-List-elements I))])
             (flprob+ p (prob-interval-measure I)))])))
