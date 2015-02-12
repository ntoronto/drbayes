#lang typed/racket/base

(require "../untyped-utils.rkt"
         "../set.rkt"
         "../flonum.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Indexes

(define-type Interval-Splitter (-> Nonempty-Prob-Interval (Listof Nonempty-Prob-Interval)))

(struct: random-index ([index : Store-Index]
                       [split : (U #f Positive-Integer Interval-Splitter)])
  #:transparent)

(struct: ifte*-index ([index : Store-Index]
                      [true  : (Promise Indexes)]
                      [false : (Promise Indexes)])
  #:transparent)

(define-type Indexes (Listof (U random-index ifte*-index)))

;; ===================================================================================================
;; Preimage mappings

(define-singleton-type Empty-Pre-Mapping empty-pre-mapping)

(struct: nonempty-pre-mapping ([range : Nonempty-Set]
                               [fun : (-> Nonempty-Set (Values Set Boolean))])
  #:transparent)

(define-type Pre-Mapping (U Empty-Pre-Mapping nonempty-pre-mapping))

;; ===================================================================================================
;; Bottom and preimage arrow

(define-type Bot-Arrow (-> Value Maybe-Value))
(define-type Pre-Arrow (-> Nonempty-Set Pre-Mapping))

;; ===================================================================================================
;; Bottom* and Preimage* arrow

(struct: bot*-arrow ([arrow : Bot-Arrow]) #:transparent)
(struct: bot-wrapper ([arrow : Bot-Arrow] [arrow* : Bot-Arrow]) #:transparent)

(struct: pre*-arrow ([arrow : Pre-Arrow]) #:transparent)
(struct: pre-wrapper ([arrow : Pre-Arrow] [arrow* : Pre-Arrow]) #:transparent)

(define-type Bot*-Arrow (U bot-wrapper bot*-arrow))
(define-type Pre*-Arrow (U pre-wrapper pre*-arrow))
(define-type Idx-Arrow (-> Store-Index Indexes))
