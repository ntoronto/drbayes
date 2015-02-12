#lang typed/racket

(require drbayes/private/set
         "../random-sets/random-null-set.rkt"
         "../test-utils.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    eq?
    null-set-subseteq?
    null-set-join
    ((inst intersect->meet Null-Set) null-set-intersect)
    empty-null-set
    nulls
    random-null-set)
   (check-membership-lattice
    empty-null-set?
    null-set-member?
    null-set-subseteq?
    null-set-join
    ((inst intersect->meet Null-Set) null-set-intersect)
    random-null-set
    random-null)
   (check-set-algebra
    eq?
    null-set-member?
    null-set-subseteq?
    empty-null-set
    nulls
    null-set-subtract
    ((inst join->union Null-Set) 'null-set-join null-set-join)
    null-set-intersect
    random-null-set
    random-null)
   ))
