#lang typed/racket/base

(require typed/untyped-utils
         "set/types.rkt"
         "set/bottom.rkt"
         "set/real-set.rkt"
         "set/prob-set.rkt"
         "set/bool-set.rkt"
         "set/null-set.rkt"
         "set/extremal-set.rkt"
         "set/store-index.rkt"
         "set/store.rkt"
         "set/store-set.rkt"
         "set/union.rkt"
         "set/value.rkt"
         "set/union-ops.rkt"
         (except-in "set/union-more-ops.rkt"
                    set-pair
                    set-list
                    set-list*
                    set-tag)
         )

(provide (all-from-out
          "set/types.rkt"
          "set/bottom.rkt"
          "set/real-set.rkt"
          "set/prob-set.rkt"
          "set/bool-set.rkt"
          "set/null-set.rkt"
          "set/extremal-set.rkt"
          "set/store-index.rkt"
          "set/store.rkt"
          "set/store-set.rkt"
          "set/union.rkt"
          "set/value.rkt"
          "set/union-ops.rkt"
          "set/union-more-ops.rkt"
          )
         set-pair
         set-list
         set-list*
         set-tag)

(require/untyped-contract
 (begin (require (only-in "set/types.rkt" Tag))
        (require (only-in "set/extremal-set.rkt"
                          Empty-Set))
        (require (only-in "set/union.rkt"
                          Nonempty-Pair-Set
                          Nonempty-Set
                          Set
                          Bot-Tagged)))
 "set/union-more-ops.rkt"
 [set-pair  (-> Set Set (U Empty-Set Nonempty-Pair-Set))]
 [set-list  (-> Set * Set)]
 [set-list*  (-> Set Set * Set)]
 [set-tag  (-> Set Tag (U Bot-Tagged Empty-Set))])
