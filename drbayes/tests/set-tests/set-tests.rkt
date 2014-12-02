#lang typed/racket

(require drbayes/private/set
         "../random-sets/random-set.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-membership-lattice
    empty-set?
    set-member?
    set-subseteq?
    set-join
    set-intersect
    random-set
    random-set-member)
   (check-bounded-lattice
    set-equal?
    set-subseteq?
    set-join
    set-intersect
    empty-set
    universe
    random-set)))
