#lang typed/racket/base

(require racket/list
         racket/match
         racket/promise
         "bottom.rkt"
         "store-index.rkt")

(provide (all-defined-out))

(struct Store ([random : (Promise Flonum)]
               [branch : (Promise (U Boolean Bottom))]
               [left   : (Promise Store)]
               [right  : (Promise Store)])
  #:transparent)

(define store? Store?)

(: store-random (-> Store Flonum))
(: store-branch (-> Store (U Boolean Bottom)))
(: store-left   (-> Store Store))
(: store-right  (-> Store Store))

(define (store-random t) (force (Store-random t)))
(define (store-branch t) (force (Store-branch t)))
(define (store-left   t) (force (Store-left   t)))
(define (store-right  t) (force (Store-right  t)))

(: store-random-ref (-> Store Store-Index Flonum))
(define (store-random-ref t j)
  (let loop ([t t] [j  (reverse j)])
    (cond [(empty? j)  (store-random t)]
          [(first j)  (loop (store-left  t) (rest j))]
          [else       (loop (store-right t) (rest j))])))

(: store-branch-ref (-> Store Store-Index (U Boolean Bottom)))
(define (store-branch-ref t j)
  (let loop ([t t] [j  (reverse j)])
    (cond [(empty? j)  (store-branch t)]
          [(first j)  (loop (store-left  t) (rest j))]
          [else       (loop (store-right t) (rest j))])))

(: store-random-list (-> Store (Listof Flonum)))
(define (store-random-list t)
  (let loop ([t t])
    (match-define (Store x _ l r) t)
    (append (if (promise-forced? x) (list (force x)) empty)
            (if (promise-forced? l) (loop (force l)) empty)
            (if (promise-forced? r) (loop (force r)) empty))))
