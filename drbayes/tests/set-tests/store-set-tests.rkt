#lang typed/racket

(require drbayes/private/set
         "../random-sets/random-store-set.rkt"
         "../test-utils.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    equal?
    store-set-subseteq?
    store-set-join
    ((inst intersect->meet Store-Set) store-set-intersect)
    empty-store-set
    stores
    random-store-set)
   (check-membership-lattice
    empty-store-set?
    store-set-member?
    store-set-subseteq?
    store-set-join
    ((inst intersect->meet Store-Set) store-set-intersect)
    random-store-set
    random-store)))
