#lang typed/racket/base

;; Empty set and universal set

(require "../untyped-utils.rkt")

(provide Empty-Set empty-set empty-set?
         Universe  universe  universe?)

(define-singleton-type Empty-Set empty-set)
(define-singleton-type Universe universe)
