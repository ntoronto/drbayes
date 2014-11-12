#lang typed/racket/base

(provide (all-defined-out))

(define-type Store-Index (Listof Boolean))

(: j0 Store-Index)
(: left (Store-Index -> Store-Index))
(: right (Store-Index -> Store-Index))

(define j0 null)
(define (left j) (cons #t j))
(define (right j) (cons #f j))
