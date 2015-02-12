#lang typed/racket

(require drbayes/private/set
         "../random-sets/random-set.rkt"
         "../test-utils.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(check-absorption-exactness? #f)
(check-exactness-commutativity? #f)

(time
 (for ([_  (in-range 100000)])
   (check-bounded-lattice
    set-equal?
    set-subseteq?
    set-join
    ((inst intersect->meet Set) set-intersect)
    empty-set
    universe
    random-set)
   (check-membership-lattice
    empty-set?
    set-member?
    set-subseteq?
    set-join
    ((inst intersect->meet Set) set-intersect)
    random-set
    random-set-member)))
