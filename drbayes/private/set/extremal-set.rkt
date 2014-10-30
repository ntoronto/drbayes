#lang typed/racket/base

;; Empty set and universal set

(provide Empty-Set empty-set empty-set? not-empty-set?
         Universe  universe  universe?  not-universe?)

(require "../untyped-utils.rkt")

(define-singleton-type Empty-Set empty-set)
(define (not-empty-set? A) (not (empty-set? A)))

(define-singleton-type Universe universe)
(define (not-universe? A) (not (universe? A)))
