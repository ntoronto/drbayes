#lang typed/racket

(require drbayes/private/set/integer-set
         "../random-sets/random-integer-set.rkt"
         "../test-utils.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    equal?
    integer-set-subseteq?
    integer-set-join
    ((inst intersect->meet Integer-Set) integer-set-intersect)
    empty-integer-set
    integers
    random-integer-set)
   (check-membership-lattice
    empty-integer-set?
    integer-set-member?
    integer-set-subseteq?
    integer-set-join
    ((inst intersect->meet Integer-Set) integer-set-intersect)
    random-integer-set
    random-integer)))
