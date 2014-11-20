#lang typed/racket

(require math/distributions
         drbayes/private/set/integer-set
         "../test-utils.rkt")

(provide (all-defined-out))

;; Using a discrete distribution for interval endpoints makes it more likely that two endpoints from
;; different intervals will be the same but one open and the other closed
(: integer-endpoint-dist (Discrete-Dist Extended-Integer))
(define integer-endpoint-dist (discrete-dist (list -inf -4 -3 -2 -1 0 1 2 3 4 +inf)))
(define random-integers '(-5 -4 -3 -2 -1 0 1 2 3 4 5))

(: int-min (-> Extended-Integer Extended-Integer Extended-Integer))
(define (int-min a b)
  (if (int< a b) a b))

(: int-max (-> Extended-Integer Extended-Integer Extended-Integer))
(define (int-max a b)
  (if (int< a b) b a))

(: random-interval (-> (Discrete-Dist Extended-Integer) Integer-Interval))
(define (random-interval dist)
  (define a (sample dist))
  (define b (sample dist))
  (define a? (< (random) 0.5))
  (define b? (< (random) 0.5))
  (integer-interval (int-min a b) (int-max a b) a? b?))

(: random-integer (-> Integer-Set Integer))
(define (random-integer I)
  (cond [(empty-integer-set? I)  (raise-argument-error 'random-integer "Nonempty-Integer-Set" I)]
        [else  (random-element (filter (λ: ([x : Integer]) (integer-set-member? I x))
                                       random-integers))]))

(: random-integer-set (->* [] [(Discrete-Dist Extended-Integer)] Integer-Set))
(define (random-integer-set [dist integer-endpoint-dist])
  (define I (random-interval dist))
  (cond [((random) . < . 0.5)  I]
        [else  (integer-set-join I (random-integer-set dist))]))
