#lang typed/racket

(require "../../private/set.rkt"
         "../../private/untyped-utils.rkt"
         "../../private/utils.rkt"
         "../random-sets/random-real-set.rkt"
         "../random-sets/random-bool-set.rkt"
         "../test-utils.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(: random-pair-set (-> Pair-Set))
(define (random-pair-set)
  (define r (random))
  (cond [(r . < . 0.1)  pairs]
        [(r . < . 0.2)  empty-pair-set]
        [else
         (let loop ()
           (define A (pair-set (bot-basic (random-real-set)) (bot-basic (random-bool-set))))
           (if (or (pairs? A) (empty-pair-set? A)) (loop) A))]))

(: random-value (Pair-Set -> (Pair Flonum Boolean)))
(define (random-value A)
  (cond [(empty-pair-set? A)  (cons +nan.0 #f)]
        [(pairs? A)   (cons (random-real reals) (random-bool bools))]
        [else
         (let ([A1  (set-take-reals (Nonextremal-Pair-Set-fst A))]
               [A2  (set-take-bools (Nonextremal-Pair-Set-snd A))])
           (cons (random-real A1) (random-bool A2)))]))

(time
 (for: ([_  (in-range 100000)])
   (check-membership-lattice
    empty-pair-set?
    pair-set-member?
    pair-set-subseteq?
    pair-set-join
    pair-set-intersect
    random-pair-set
    random-value)
   (check-bounded-lattice
    equal?
    pair-set-subseteq?
    pair-set-join
    pair-set-intersect
    empty-pair-set
    pairs
    random-pair-set)))