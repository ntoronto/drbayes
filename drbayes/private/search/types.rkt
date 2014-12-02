#lang typed/racket/base

(require "../set.rkt"
         "../flonum.rkt"
         "../arrow.rkt"
         "search-tree.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(struct: store-set-sample ([set : Nonempty-Store-Set] [numer : Prob] [denom : Prob])
  #:transparent)

(struct: store-sample ([s : Store] [numer : Prob] [denom : Prob])
  #:transparent)

;; ===================================================================================================

(define-type Store-Search-Tree (Search-Tree Store-Set))

;; ===================================================================================================

(define-type Ann-Index (U ann-random-index ann-ifte*-index))
(define-type Ann-Indexes (Listof Ann-Index))

(struct: ann-random-index ([index : Store-Index]
                           [split : (U #f Interval-Splitter)]
                           [num-splits : Natural])
  #:transparent)

(struct: ann-ifte*-index ([index : Store-Index]
                          [true : (Promise Ann-Indexes)]
                          [false : (Promise Ann-Indexes)])
  #:transparent)
