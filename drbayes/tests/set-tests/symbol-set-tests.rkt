#lang typed/racket

(require drbayes/private/set/symbol-set
         "../random-sets/random-symbol-set.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-set-algebra
    equal?
    symbol-set-member?
    symbol-set-subseteq?
    empty-symbol-set
    full-symbol-set
    symbol-set-subtract
    symbol-set-union
    symbol-set-intersect
    random-symbol-set
    random-symbol)
   (check-bounded-lattice
    equal?
    symbol-set-subseteq?
    symbol-set-union
    symbol-set-intersect
    empty-symbol-set
    full-symbol-set
    random-symbol-set)))
