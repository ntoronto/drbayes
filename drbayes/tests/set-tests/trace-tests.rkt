#lang typed/racket

(require typed/rackunit
         math/distributions
         "../../private/set/bool-set.rkt"
         "../../private/set/tree-value.rkt"
         "../../private/set/tree-set.rkt"
         "../random-sets/random-trace-set.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    equal?
    trace-set-subseteq?
    trace-set-join
    trace-set-intersect
    empty-trace-set
    traces
    random-trace-set)
   ((inst check-membership-lattice Trace-Set Trace)
    empty-trace-set?
    trace-set-member?
    trace-set-subseteq?
    trace-set-join
    trace-set-intersect
    random-trace-set
    (λ (A)
      (cond [(empty-trace-set? A)
             (raise-argument-error 'trace-set-sample-point "Nonempty-Trace-Set" A)]
            [else
             (trace-set-sample-point A)])))))
