#lang typed/racket/base

(require racket/match
         racket/promise
         "../set.rkt"
         "../flonum.rkt"
         "types.rkt"
         "indexes.rkt"
         "pure-arrows.rkt")

(provide (all-defined-out))

(: η/bot* (Bot-Arrow -> Bot*-Arrow))
(define (η/bot* f)
  (bot-wrapper f (>>>/bot (snd/bot) f)))

(: η/pre* (Pre-Arrow -> Pre*-Arrow))
(define (η/pre* h)
  (pre-wrapper h (>>>/pre (snd/pre) h)))

(: run/bot* (Bot*-Arrow -> Bot-Arrow))
(define (run/bot* k)
  (if (bot-wrapper? k)
      (bot-wrapper-arrow* k)
      (bot*-arrow-arrow k)))

(: run/pre* (Pre*-Arrow -> Pre-Arrow))
(define (run/pre* k)
  (if (pre-wrapper? k)
      (pre-wrapper-arrow* k)
      (pre*-arrow-arrow k)))

(: any/idx Idx-Arrow)
(define (any/idx j) '())

;; ===================================================================================================
;; Basic computable lifts

(define (fail/bot*) (η/bot* (fail/bot)))
(define (fail/pre*) (η/pre* (fail/pre)))
(define (fail/idx) any/idx)

(define (id/bot*) (η/bot* (id/bot)))
(define (id/pre*) (η/pre* (id/pre)))
(define (id/idx) any/idx)

(define restrict/bot* (λ: ([X : Nonempty-Set]) (η/bot* (restrict/bot X))))
(define restrict/pre* (λ: ([X : Nonempty-Set]) (η/pre* (restrict/pre X))))
(define restrict/idx (λ: ([X : Nonempty-Set]) any/idx))

(define const/bot* (λ: ([b : Value]) (η/bot* (const/bot b))))
(define const/pre* (λ: ([b : Value]) (η/pre* (const/pre b))))
(define const/idx (λ: ([b : Value]) any/idx))

(define (fst/bot*) (η/bot* (fst/bot)))
(define (fst/pre*) (η/pre* (fst/pre)))
(define (fst/idx) any/idx)

(define (snd/bot*) (η/bot* (snd/bot)))
(define (snd/pre*) (η/pre* (snd/pre)))
(define (snd/idx) any/idx)

(define list-ref/bot* (λ: ([j : Natural]) (η/bot* (list-ref/bot j))))
(define list-ref/pre* (λ: ([j : Natural]) (η/pre* (list-ref/pre j))))
(define list-ref/idx (λ: ([j : Natural]) any/idx))

;; ===================================================================================================
;; Arrow combinators

;; ---------------------------------------------------------------------------------------------------
;; Composition

(: >>>/bot* (Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (>>>/bot* k1 k2)
  (cond [(and (bot-wrapper? k1) (bot-wrapper? k2))
         (η/bot* (>>>/bot (bot-wrapper-arrow k1) (bot-wrapper-arrow k2)))]
        [else
         (bot*-arrow
          (>>>/bot (&&&/bot (>>>/bot (fst/bot) (store-right/bot))
                            (>>>/bot (first/bot (store-left/bot)) (run/bot* k1)))
                   (run/bot* k2)))]))

(: >>>/pre* (Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (>>>/pre* k1 k2)
  (cond [(and (pre-wrapper? k1) (pre-wrapper? k2))
         (η/pre* (>>>/pre (pre-wrapper-arrow k1) (pre-wrapper-arrow k2)))]
        [else
         (pre*-arrow
          (>>>/pre (&&&/pre (>>>/pre (fst/pre) (store-right/pre))
                            (>>>/pre (first/pre (store-left/pre)) (run/pre* k1)))
                   (run/pre* k2)))]))

(: >>>/idx (Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((>>>/idx k1 k2) j)
  (append (k1 (left j)) (k2 (right j))))

;; ---------------------------------------------------------------------------------------------------
;; Pairing

(: &&&/bot* (Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (&&&/bot* k1 k2)
  (cond [(and (bot-wrapper? k1) (bot-wrapper? k2))
         (η/bot* (&&&/bot (bot-wrapper-arrow k1) (bot-wrapper-arrow k2)))]
        [else
         (bot*-arrow
          (&&&/bot (>>>/bot (first/bot (store-left/bot)) (run/bot* k1))
                   (>>>/bot (first/bot (store-right/bot)) (run/bot* k2))))]))

(: &&&/pre* (Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (&&&/pre* k1 k2)
  (cond [(and (pre-wrapper? k1) (pre-wrapper? k2))
         (η/pre* (&&&/pre (pre-wrapper-arrow k1) (pre-wrapper-arrow k2)))]
        [else
         (pre*-arrow
          (&&&/pre (>>>/pre (first/pre (store-left/pre)) (run/pre* k1))
                   (>>>/pre (first/pre (store-right/pre)) (run/pre* k2))))]))

(: &&&/idx (Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((&&&/idx k1 k2) j)
  (append (k1 (left j)) (k2 (right j))))

;; ---------------------------------------------------------------------------------------------------
;; Partial if-then-else

(: ifte/bot* (Bot*-Arrow Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (ifte/bot* k1 k2 k3)
  (cond
    [(and (bot-wrapper? k1) (bot-wrapper? k2) (bot-wrapper? k3))
     (η/bot* (ifte/bot (bot-wrapper-arrow k1)
                       (bot-wrapper-arrow k2)
                       (bot-wrapper-arrow k3)))]
    [else
     (bot*-arrow
      (ifte/bot (>>>/bot (first/bot (store-left/bot)) (run/bot* k1))
                (>>>/bot (first/bot (>>>/bot (store-right/bot) (store-left/bot))) (run/bot* k2))
                (>>>/bot (first/bot (>>>/bot (store-right/bot) (store-right/bot))) (run/bot* k3))))]))

(: ifte/pre* (Pre*-Arrow Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (ifte/pre* k1 k2 k3)
  (cond
    [(and (pre-wrapper? k1) (pre-wrapper? k2) (pre-wrapper? k3))
     (η/pre* (ifte/pre (pre-wrapper-arrow k1)
                       (pre-wrapper-arrow k2)
                       (pre-wrapper-arrow k3)))]
    [else
     (pre*-arrow
      (ifte/pre (>>>/pre (first/pre (store-left/pre)) (run/pre* k1))
                (>>>/pre (first/pre (>>>/pre (store-right/pre) (store-left/pre))) (run/pre* k2))
                (>>>/pre (first/pre (>>>/pre (store-right/pre) (store-right/pre))) (run/pre* k3))))]))

(: ifte/idx (Idx-Arrow Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((ifte/idx k1 k2 k3) j)
  (append (k1 (left j))
          (k2 (left (right j)))
          (k3 (right (right j)))))

;; ---------------------------------------------------------------------------------------------------
;; Laziness

(: lazy/bot* ((Promise Bot*-Arrow) -> Bot*-Arrow))
(define (lazy/bot* k)
  (bot*-arrow (lazy/bot (delay (run/bot* (force k))))))

(: lazy/pre* ((Promise Pre*-Arrow) -> Pre*-Arrow))
(define (lazy/pre* k)
  (pre*-arrow (lazy/pre (delay (run/pre* (force k))))))

(: lazy/idx ((Promise Idx-Arrow) -> Idx-Arrow))
(define ((lazy/idx k) j) ((force k) j))

;; ===================================================================================================
;; Random source and branch trace projections

(: proj-domain-fail (Symbol Value -> Bottom))
(define (proj-domain-fail name a)
  (bottom (delay (format "~a: expected value in program domain; given ~e" name a))))

;; ---------------------------------------------------------------------------------------------------
;; Branch trace projections

(: store-branch/bot* (-> Bot*-Arrow))
(define (store-branch/bot*)
  (bot*-arrow
   (λ: ([a : Value])
     (match a
       [(cons (? store? s) _)  (store-branch s)]
       [_  (proj-domain-fail 'if a)]))))

(: store-branch/pre* (-> Pre*-Arrow))
(define (store-branch/pre*)
  (pre*-arrow (>>>/pre (fst/pre) (store-branch/pre))))

;; ---------------------------------------------------------------------------------------------------
;; Random source projections

(: store-uniform/bot* (-> Bot*-Arrow))
(define (store-uniform/bot*)
  (bot*-arrow (>>>/bot (fst/bot) (store-uniform/bot))))

(: store-uniform/pre* (-> Pre*-Arrow))
(define (store-uniform/pre*)
  (pre*-arrow (>>>/pre (fst/pre) (store-uniform/pre))))

(: store-uniform/idx (-> Idx-Arrow))
(define ((store-uniform/idx) j)
  (list (random-index j #f)))

;; ---------------------------------------------------------------------------------------------------
;; Random source boolean projections

(: boolean/bot* (Flonum -> Bot*-Arrow))
(define (boolean/bot* p)
  (bot*-arrow (>>>/bot (fst/bot) (store-boolean/bot p))))

(: boolean/pre* (Flonum -> Pre*-Arrow))
(define (boolean/pre* p)
  (pre*-arrow (>>>/pre (fst/pre) (store-boolean/pre p))))

(: boolean/idx (Flonum -> Idx-Arrow))
(define (boolean/idx p)
  (cond [(and (p . > . 0.0) (p . < . 1.0))
         (define-values (Xt Xf) (boolean-preimage p))
         (define split (make-constant-splitter (list Xt Xf)))
         (λ (j) (list (random-index j split)))]
        [else  any/idx]))

;; ===================================================================================================
;; Total if-then-else

(: ifte*/bot* (Bot*-Arrow Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (ifte*/bot* k1 k2 k3)
  (bot*-arrow
   (ifte*/bot (run/bot* (store-branch/bot*))
              (>>>/bot (first/bot (store-left/bot)) (run/bot* k1))
              (>>>/bot (first/bot (>>>/bot (store-right/bot) (store-left/bot))) (run/bot* k2))
              (>>>/bot (first/bot (>>>/bot (store-right/bot) (store-right/bot))) (run/bot* k3)))))

(: ifte*/pre* (Pre*-Arrow Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (ifte*/pre* k1 k2 k3)
  (pre*-arrow
   (ifte*/pre (run/pre* (store-branch/pre*))
              (>>>/pre (first/pre (store-left/pre)) (run/pre* k1))
              (>>>/pre (first/pre (>>>/pre (store-right/pre) (store-left/pre))) (run/pre* k2))
              (>>>/pre (first/pre (>>>/pre (store-right/pre) (store-right/pre))) (run/pre* k3)))))

(: ifte*/idx (Idx-Arrow Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((ifte*/idx k1 k2 k3) j)
  (append (k1 (left j))
          (list (ifte*-index j
                             (delay (k2 (left (right j))))
                             (delay (k3 (right (right j))))))))
