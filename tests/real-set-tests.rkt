#lang typed/racket

(require "../private/set/real-set.rkt"
         "rackunit-utils.rkt"
         "random-real-set.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-set-algebra 
    equal?
    real-set-member?
    real-set-subseteq?
    empty-real-set
    reals
    real-set-subtract
    real-set-union
    real-set-intersect
    random-real-set
    random-real)
   (check-bounded-lattice
    equal?
    real-set-subseteq?
    real-set-union
    real-set-intersect
    empty-real-set
    reals
    random-real-set)))
