#lang typed/racket/base

(require "types.rkt"
         "proc-arrow.rkt")

(provide (all-defined-out))

(struct: meaning ([proc : Proc-Arrow]
                  [bot* : Bot*-Arrow]
                  [pre* : Pre*-Arrow]
                  [idx : Idx-Arrow])
  #:transparent)
