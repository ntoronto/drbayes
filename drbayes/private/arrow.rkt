#lang typed/racket/base

(require "arrow/indexes.rkt"
         "arrow/preimage-mapping.rkt"
         "arrow/pure-arrows.rkt"
         "arrow/pure-lifts.rkt"
         "arrow/prob-arrows.rkt"
         "arrow/prob-lifts.rkt"
         "arrow/proc-arrow.rkt")

(struct: meaning ([proc : Proc-Arrow]
                  [bot* : Bot*-Arrow]
                  [pre* : Pre*-Arrow]
                  [idx : Idx-Arrow])
  #:transparent)

(provide (all-from-out
          "arrow/indexes.rkt"
          "arrow/preimage-mapping.rkt"
          "arrow/pure-arrows.rkt"
          "arrow/pure-lifts.rkt"
          "arrow/prob-arrows.rkt"
          "arrow/prob-lifts.rkt"
          "arrow/proc-arrow.rkt")
         (all-defined-out))
