#lang typed/racket/base

(provide (rename-out [-Bottom Bottom])
         bottom
         bottom?
         bottom-message)

(struct Bottom ([message : (Promise String)]) #:transparent)

(define-type -Bottom Bottom)
(define bottom Bottom)
(define bottom? Bottom?)
(define bottom-message Bottom-message)
