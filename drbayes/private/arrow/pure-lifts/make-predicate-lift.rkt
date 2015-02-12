#lang typed/racket/base

(require racket/promise
         "../../set.rkt"
         "../types.rkt"
         "../cache.rkt")

(provide (all-defined-out))

(: predicate/bot (-> Symbol (-> Value (U Bottom Boolean)) Nonempty-Set Nonempty-Set (-> Bot-Arrow)))
(define (predicate/bot name f Xt Xf)
  (: arg-error (-> Nonempty-Set Value Bottom))
  (: res-error (-> Value Bottom))
  (define (arg-error X a) (bottom (delay (format "~a: expected argument in ~e; given ~e" name X a))))
  (define (res-error b) (bottom (delay (format "~a: expected Boolean; produced ~e" name b))))
  (λ ()
    (λ (a)
      (define b (f a))
      (cond [(bottom? b)  b]
            [(eq? b #t)  (if (set-member? Xt a) #t (arg-error Xt a))]
            [(eq? b #f)  (if (set-member? Xf a) #f (arg-error Xf a))]
            [else  (res-error b)]))))

(: predicate/pre (-> Nonempty-Set Nonempty-Set (-> Pre-Arrow)))
(define ((predicate/pre Xt Xf))
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (A)
     (define At (set-intersect A Xt))
     (define Af (set-intersect A Xf))
     (cond [(and (empty-set? At) (empty-set? Af))  empty-pre-mapping]
           [(empty-set? Af)  (nonempty-pre-mapping trues  (λ (B) (values At #t)))]
           [(empty-set? At)  (nonempty-pre-mapping falses (λ (B) (values Af #t)))]
           [else   (define-values (A A-exact?) (set-join At Af))
                   (nonempty-pre-mapping
                    bools
                    (fun (λ (B)
                           (values (cond [(trues? B)  At]
                                         [(falses? B)  Af]
                                         [else  A])
                                   #t))))]))))

(: predicate/prim (-> Symbol (-> Value (U Bottom Boolean)) Nonempty-Set Nonempty-Set
                      (Values (-> Bot-Arrow) (-> Pre-Arrow))))
(define (predicate/prim name f Xt Xf)
  (values (predicate/bot name f Xt Xf)
          (predicate/pre Xt Xf)))
