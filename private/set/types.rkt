#lang typed/racket/base

(provide (all-defined-out))

(struct: Base-Value () #:transparent)

(struct: Base-Set () #:transparent)
(define set? Base-Set?)

(struct: Base-Bot-Set Base-Set () #:transparent)
(define bot-set? Base-Bot-Set?)

(struct: Base-Top-Set Base-Set () #:transparent)
(define top-set? Base-Top-Set?)

(struct: Base-Bot-Entry Base-Bot-Set () #:transparent)
(define bot-entry? Base-Bot-Entry?)

(struct: Base-Top-Entry Base-Top-Set () #:transparent)
(define top-entry? Base-Top-Entry?)

(struct: Base-Bot-Basic Base-Bot-Entry () #:transparent)
(define bot-basic? Base-Bot-Basic?)
