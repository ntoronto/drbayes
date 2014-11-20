#lang typed/racket

(require drbayes/private/set
         "../random-sets/random-store-set.rkt"
         "set-properties.rkt")

(printf "starting...~n")

(time
 (for: ([_  (in-range 100000)])
   (check-bounded-lattice
    equal?
    store-set-subseteq?
    store-set-join
    store-set-intersect
    empty-store-set
    stores
    random-store-set)
   (check-membership-lattice
    empty-store-set?
    store-set-member?
    store-set-subseteq?
    store-set-join
    store-set-intersect
    random-store-set
    (λ ([S : Store-Set])
      (if (empty-store-set? S)
          (raise-argument-error 'store-set-realize "Nonempty-Store-Set" S)
          (store-set-realize S))))))
