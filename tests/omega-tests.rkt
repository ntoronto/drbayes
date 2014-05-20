#lang typed/racket

(require typed/rackunit
         math/distributions
         "../private/set/real-set.rkt"
         "../private/set/tree-value.rkt"
         "../private/set/tree-set.rkt"
         "rackunit-utils.rkt"
         "random-omega-set.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    equal?
    omega-set-subseteq?
    omega-set-join
    omega-set-intersect
    empty-omega-set
    omegas
    random-omega-set)
   ((inst check-membership-lattice Omega-Set Omega)
    empty-omega-set?
    omega-set-member?
    omega-set-subseteq?
    omega-set-join
    omega-set-intersect
    random-omega-set
    (λ (A)
      (cond [(empty-omega-set? A)
             (raise-argument-error 'omega-set-sample-point "Nonempty-Omega-Set" A)]
            [else
             (omega-set-sample-point A)])))))
