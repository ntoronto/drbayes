#lang typed/racket/base

(require racket/list
         (only-in typed/rackunit check-true)
         plot/typed
         "../main.rkt")

(provide (all-defined-out)
         profile-thunk
         profile-expr)

;; ===================================================================================================
;; Profiling

(require/typed
 profile
 [profile-thunk  ((-> Any) -> Void)])

(define: b : Boolean #f)

(define-syntax-rule (profile-expr e . args)
  (let* ([thnk  (λ () e)]
         [val  (if b (thnk) #f)])
    (profile-thunk (λ () (set! val (thnk))) . args)
    (assert val (λ: ([x : Any]) x))))

;; ===================================================================================================
;; Plotting utils

(: real-set->ivls (Nonempty-Real-Set -> (Listof ivl)))
(define (real-set->ivls I)
  (cond [(interval? I)  (define-values (a b a? b?) (interval-fields I))
                        (list (ivl a b))]
        [else  (append* (map real-set->ivls (interval-list-elements I)))]))

(: maybe-pad-list (All (A) ((Listof A) Integer (-> A) -> (Listof A))))
(define (maybe-pad-list lst n thnk)
  (append lst (build-list (max 0 (- n (length lst))) (λ (_) (thnk)))))

(: cons-product (All (A B) ((Listof A) (Listof B) -> (Listof (Pair A B)))))
(define (cons-product as bs)
  (let a-loop ([as as])
    (cond [(empty? as)  empty]
          [else
           (define a (first as))
           (let b-loop ([bs bs])
             (cond [(empty? bs)  (a-loop (rest as))]
                   [else  (cons (cons a (first bs)) (b-loop (rest bs)))]))])))

(: list-product (All (A) ((Listof (Listof A)) -> (Listof (Listof A)))))
(define (list-product xss)
  (cond [(empty? xss)  (list empty)]
        [else  (cons-product (first xss) (list-product (rest xss)))]))

(: store-set->plot-rects (Nonempty-Store-Set -> (Listof (Listof ivl))))
(define (store-set->plot-rects S)
  (map (λ: ([lst : (Listof ivl)])
         (maybe-pad-list lst 3 (λ () (ivl 0 1))))
       (list-product (map (λ: ([lst : (Listof ivl)]) (take lst (min (length lst) 3)))
                          (map real-set->ivls (store-set-random-list S))))))

(: store->point (Store -> (Listof Flonum)))
(define (store->point s)
  (maybe-pad-list (store-random-list s) 3 random))

(: value->listof-flonum (Maybe-Value -> (Listof Flonum)))
(define (value->listof-flonum v)
  (cond [(flonum? v)  (list v)]
        [(boolean? v)  (list (if v 1.0 0.0))]
        [(pair? v)  (append (value->listof-flonum (car v))
                            (value->listof-flonum (cdr v)))]
        [(null? v)  (list)]
        [(tagged-value? v)  (value->listof-flonum (tagged-value-value v))]
        [else  (list -1.0)]))

;; ===================================================================================================
;; Testing utils

(: random-element (All (A) ((Listof A) -> A)))
(define (random-element xs)
  (list-ref xs (random (length xs))))

;; Using this is about 1000x faster than using `check-true' directly, mostly because it doesn't have
;; to construct the message unless there's a failure
(define-syntax-rule (check-prop expr msg)
  (if expr (void) (check-true expr msg)))

(define-syntax-rule (implies a b) (or (not a) b))
