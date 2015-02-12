#lang typed/racket

(require drbayes/private/set/symbol-set
         "../random-sets/random-symbol-set.rkt"
         "../test-utils.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    equal?
    symbol-set-subseteq?
    symbol-set-join
    ((inst intersect->meet Symbol-Set) symbol-set-intersect)
    empty-symbol-set
    full-symbol-set
    random-symbol-set)
   (check-membership-lattice
    empty-symbol-set?
    symbol-set-member?
    symbol-set-subseteq?
    symbol-set-join
    ((inst intersect->meet Symbol-Set) symbol-set-intersect)
    random-symbol-set
    random-symbol)
   (check-set-algebra
    equal?
    symbol-set-member?
    symbol-set-subseteq?
    empty-symbol-set
    full-symbol-set
    symbol-set-subtract
    ((inst join->union Symbol-Set) 'symbol-set-join symbol-set-join)
    symbol-set-intersect
    random-symbol-set
    random-symbol)))
