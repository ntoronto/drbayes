#lang typed/racket

(require "../private/set/null-set.rkt"
         "rackunit-utils.rkt"
         "random-null-set.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-set-algebra
    eq?
    null-set-member?
    null-set-subseteq?
    empty-null-set
    nulls
    null-set-subtract
    null-set-union
    null-set-intersect
    random-null-set
    random-null)
   (check-bounded-lattice
    eq?
    null-set-subseteq?
    null-set-union
    null-set-intersect
    empty-null-set
    nulls
    random-null-set)))
