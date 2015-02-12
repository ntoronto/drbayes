#lang typed/racket/base

(require "../set.rkt"
         "../flonum.rkt"
         "../arrow.rkt"
         "search-tree.rkt")

(provide (all-defined-out))

(struct: store-set-sample ([set : Nonempty-Store-Set] [numer : Prob] [denom : Prob])
  #:transparent)

(struct: store-sample ([s : Store] [numer : Prob] [denom : Prob])
  #:transparent)

(define-type Store-Search-Tree (Search-Tree Nonempty-Store-Set Empty-Store-Set))
