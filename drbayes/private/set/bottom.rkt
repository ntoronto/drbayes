#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))

(struct: Bottom ([message : (Promise String)]) #:transparent)

(define-syntax bottom (make-rename-transformer #'Bottom))
(define-syntax bottom? (make-rename-transformer #'Bottom?))
(define-syntax bottom-message (make-rename-transformer #'Bottom-message))
