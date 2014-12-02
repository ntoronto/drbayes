#lang typed/racket/base

(require racket/list
         racket/match
         racket/promise
         "bottom.rkt"
         "store-index.rkt"
         "../flonum.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(struct Store ([random : (Maybe-Promise (U Prob Bad-Prob))]
               [branch : (Maybe-Promise (U Boolean Bottom))]
               [left   : (Maybe-Promise Store)]
               [right  : (Maybe-Promise Store)])
  #:transparent)

(define store? Store?)

(: store-random (-> Store (U Prob Bad-Prob)))
(: store-branch (-> Store (U Boolean Bottom)))
(: store-left   (-> Store Store))
(: store-right  (-> Store Store))

(define (store-random t) (maybe-force (Store-random t)))
(define (store-branch t) (maybe-force (Store-branch t)))
(define (store-left   t) (maybe-force (Store-left   t)))
(define (store-right  t) (maybe-force (Store-right  t)))

(: store-random-list (-> Store (Listof (U Bad-Prob Prob))))
(define (store-random-list t)
  (let loop ([t t])
    (match-define (Store x _ l r) t)
    (append (if (promise? x) empty (list x))
            (if (promise? l) empty (loop l))
            (if (promise? r) empty (loop r)))))
