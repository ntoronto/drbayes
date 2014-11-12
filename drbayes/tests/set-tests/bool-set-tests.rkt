#lang typed/racket

(require drbayes/private/set
         "../random-sets/random-bool-set.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-set-algebra
    eq?
    bool-set-member?
    bool-set-subseteq?
    empty-bool-set
    bools
    bool-set-subtract
    bool-set-union
    bool-set-intersect
    random-bool-set
    random-bool)
   (check-bounded-lattice
    eq?
    bool-set-subseteq?
    bool-set-union
    bool-set-intersect
    empty-bool-set
    bools
    random-bool-set)))
