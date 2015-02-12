#lang typed/racket/base

(require racket/promise
         "../../set.rkt"
         "../types.rkt"
         "../cache.rkt"
         "bijection.rkt"
         "trijection.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; R -> R lifts

(: real/bot (-> Symbol Nonempty-Real-Set Nonempty-Set (-> Flonum Value) (-> Bot-Arrow)))
(define (real/bot name X Y f)
  (: arg-error (-> Value Bottom))
  (: res-error (-> Value Bottom))
  (define (arg-error a) (bottom (delay (format "~a: expected argument in ~e; given ~e" name X a))))
  (define (res-error b) (bottom (delay (format "~a: expected result in ~e; produced ~e" name Y b))))
  (λ ()
    (λ (a)
      (cond [(and (real? a) (real-set-member? X a))
             (define b (f a))
             (cond [(set-member? Y b)  b]
                   [else
                    (res-error b)])]
            [else
             (arg-error a)]))))

;; ---------------------------------------------------------------------------------------------------
;; Strictly monotone and surjective (i.e. invertible)

(: strict-monotone/pre (-> bijection (-> Pre-Arrow)))
(define (strict-monotone/pre f)
  (define img (bijection-image f))
  (define pre (bijection-preimage f))
  (λ ()
    (define fun (make-pre-mapping-fun/memo))
    (make-pre-arrow/memo
     (λ (A)
       (define-values (B B-exact?) (img A))
       (cond [(empty-set? B)  empty-pre-mapping]
             [else
              (define h (pre A))
              (nonempty-pre-mapping
               B (fun (λ (B)
                        (define-values (A A-exact?) (h B))
                        (values A A-exact?))))])))))

(: strict-monotone/prim (-> Symbol (-> Flonum Value) bijection
                            (Values (-> Bot-Arrow) (-> Pre-Arrow))))
(define (strict-monotone/prim name f/proc f)
  (values (real/bot name (bijection-domain f) (bijection-range f) f/proc)
          (strict-monotone/pre f)))

;; ---------------------------------------------------------------------------------------------------
;; Not strictly monotone

(: monotone/pre (-> Nonempty-Real-Set Nonempty-Real-Set
                    (-> Nonempty-Real-Interval (Values Real-Set Boolean))
                    (-> Nonempty-Real-Interval (Values Real-Set Boolean))
                    (-> Pre-Arrow)))
(define ((monotone/pre X Y img pre))
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
  (λ (A)
    (let*-values ([(A)  (set-intersect A X)]
                  [(B B-exact?)  (monotone-apply img A)]
                  [(B)  (set-intersect Y B)])
      (cond [(empty-set? B)  empty-pre-mapping]
            [else
             (nonempty-pre-mapping
              B (fun (λ (B)
                       (define-values (C C-exact?) (monotone-apply pre B))
                       (values (set-intersect A C) (and B-exact? C-exact?)))))])))))

(: monotone/prim (-> Symbol
                     Nonempty-Real-Set Nonempty-Real-Set
                     (-> Flonum Value)
                     (-> Nonempty-Real-Interval (Values Real-Set Boolean))
                     (-> Nonempty-Real-Interval (Values Real-Set Boolean))
                     (Values (-> Bot-Arrow) (-> Pre-Arrow))))
(define (monotone/prim name X Y f img pre)
  (values (real/bot name X Y f)
          (monotone/pre X Y img pre)))

;; ===================================================================================================
;; R x R -> R lifts

(: real2d/bot (-> Symbol Nonempty-Real-Set Nonempty-Real-Set Nonempty-Set (-> Flonum Flonum Value)
                  (-> Bot-Arrow)))
(define (real2d/bot name X1 X2 Y f)
  (define X (set-pair X1 X2))
  (: arg-error (-> Value Bottom))
  (: res-error (-> Value Bottom))
  (define (arg-error a) (bottom (delay (format "~a: expected argument in ~e; given ~e" name X a))))
  (define (res-error b) (bottom (delay (format "~a: expected result in ~e; produced ~e" name Y b))))
  (λ ()
    (λ (a)
      (if (pair? a)
          (let ([a1  (car a)]
                [a2  (cdr a)])
            (if (and (real? a1) (real? a2) (real-set-member? X1 a1) (real-set-member? X2 a2))
                (let ([b  (f a1 a2)])
                  (if (set-member? Y b) b (res-error b)))
                (arg-error a)))
          (arg-error a)))))

;; ---------------------------------------------------------------------------------------------------
;; Strictly monotone and surjective (i.e. axis-invertible)

(: strict-monotone2d/pre (-> trijection (-> Pre-Arrow)))
(define (strict-monotone2d/pre f)
  (define img (trijection-image f))
  (define pre (trijection-preimage f))
  (λ ()
    (define fun (make-pre-mapping-fun/memo))
    (make-pre-arrow/memo
     (λ (A)
       (define-values (B B-exact?) (img A))
       (cond [(empty-set? B)  empty-pre-mapping]
             [else
              (define h (pre A))
              (nonempty-pre-mapping
               B (fun (λ (B)
                        (define-values (A A-exact?) (h B))
                        (values A A-exact?))))])))))

(: strict-monotone2d/prim (-> Symbol (-> Flonum Flonum Value) trijection
                              (Values (-> Bot-Arrow) (-> Pre-Arrow))))
(define (strict-monotone2d/prim name f/proc f)
  (values (real2d/bot name (trijection-domain1 f) (trijection-domain2 f) (trijection-range f) f/proc)
          (strict-monotone2d/pre f)))
