#lang typed/racket

(require drbayes/private/set
         "../random-sets/random-real-set.rkt"
         "../test-utils.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    equal?
    real-set-subseteq?
    real-set-join
    ((inst intersect->meet Real-Set) real-set-intersect)
    empty-real-set
    reals
    random-real-set)
   (check-membership-lattice
    empty-real-set?
    real-set-member?
    real-set-subseteq?
    real-set-join
    ((inst intersect->meet Real-Set) real-set-intersect)
    random-real-set
    random-real)))
