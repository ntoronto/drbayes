#lang typed/racket/base

(require racket/promise
         "../../set.rkt"
         "../../flonum.rkt"
         "../types.rkt"
         "../cache.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(: inverse-cdf/bot (-> Symbol Nonempty-Set (-> Flonum Value) (-> Bot-Arrow)))
(define (inverse-cdf/bot name Y f)
  (: arg-error (-> Value Bottom))
  (: res-error (-> Value Bottom))
  (define (arg-error a) (bottom (delay (format "~a: expected probability; given ~e" name a))))
  (define (res-error b) (bottom (delay (format "~a: expected result in ~e; produced ~e" name Y b))))
  (λ ()
    (λ (a)
      (cond [(prob? a)
             (define b (f (Prob-value a)))
             (cond [(set-member? Y b)  b]
                   [else
                    (res-error b)])]
            [else
             (arg-error a)]))))

;; ===================================================================================================

(: inverse-cdf-img (-> Nonempty-Real-Set (-> Flonum Flonum) (-> Flonum Flonum)
                       (-> Set (Values Set Boolean))))
(define ((inverse-cdf-img Y f/rndd f/rndu) A)
  (define-values (B B-exact?)
    (prob-set-map*
     (λ (A)
       (define-values (a1 a2 a1? a2?) (prob-interval-fields A))
       (define B (real-interval (f/rndd (Prob-value a1))
                                (f/rndu (Prob-value a2))
                                a1?
                                a2?))
       (values (if (empty-real-set? B) empty-set B)
               #t))
     (set-take-probs A)))
  (values (set-intersect Y B) B-exact?))

(: inverse-cdf-pre (-> Nonempty-Real-Set (-> Flonum Flonum) (-> Flonum Flonum)
                       (-> Set (-> Set (Values Set Boolean)))))
(define ((inverse-cdf-pre Y f/rndd f/rndu) A)
  (let ([A  (set-intersect A probs)])
    (λ (B)
      (define-values (C C-exact?)
        (real-set-map*
         (λ (B)
           (define-values (b1 b2 b1? b2?) (real-interval-fields B))
           (define A (prob-interval (Prob (f/rndd b1))
                                    (Prob (f/rndu b2))
                                    b1?
                                    b2?))
           (values (if (empty-prob-set? A) empty-set A)
                   #t))
         (set-take-reals (set-intersect B Y))))
      (values (set-intersect C A) C-exact?))))

(: inverse-cdf-img/fake-rnd (-> Nonempty-Real-Set (-> Flonum Flonum) Index
                                (-> Set (Values Set Boolean))))
(define (inverse-cdf-img/fake-rnd Y f n)
  (inverse-cdf-img Y (λ (x) (flstep* (f x) (- n))) (λ (x) (flstep* (f x) n))))

(: inverse-cdf-pre/fake-rnd (-> Nonempty-Real-Set (-> Flonum Flonum) Index
                                (-> Set (-> Set (Values Set Boolean)))))
(define (inverse-cdf-pre/fake-rnd Y f n)
  (inverse-cdf-pre Y
                   (λ (y) (flprob-fast-canonicalize (flstep* (f y) (- n))))
                   (λ (y) (flprob-fast-canonicalize (flstep* (f y) n)))))

(: make-pre-arrow (-> (-> Nonempty-Set (Values Set Boolean))
                      (-> Nonempty-Set (-> Nonempty-Set (Values Set Boolean)))
                      (-> Pre-Arrow)))
(define (make-pre-arrow img pre)
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
                        (values A (and A-exact? B-exact?)))))])))))
