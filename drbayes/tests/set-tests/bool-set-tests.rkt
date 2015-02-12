#lang typed/racket

(require drbayes/private/set
         "../random-sets/random-bool-set.rkt"
         "../test-utils.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    eq?
    bool-set-subseteq?
    bool-set-join
    ((inst intersect->meet Bool-Set) bool-set-intersect)
    empty-bool-set
    bools
    random-bool-set)
   (check-membership-lattice
    empty-bool-set?
    bool-set-member?
    bool-set-subseteq?
    bool-set-join
    ((inst intersect->meet Bool-Set) bool-set-intersect)
    random-bool-set
    random-bool)
   (check-set-algebra
    eq?
    bool-set-member?
    bool-set-subseteq?
    empty-bool-set
    bools
    bool-set-subtract
    ((inst join->union Bool-Set) 'bool-set-join bool-set-join)
    bool-set-intersect
    random-bool-set
    random-bool)))
