#lang typed/racket

(require drbayes/private/set
         "../random-sets/random-prob-set.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    equal?
    prob-set-subseteq?
    prob-set-join
    prob-set-intersect
    empty-prob-set
    probs
    random-prob-set)
   (check-membership-lattice
    empty-prob-set?
    prob-set-member?
    prob-set-subseteq?
    prob-set-join
    prob-set-intersect
    random-prob-set
    random-prob)))
