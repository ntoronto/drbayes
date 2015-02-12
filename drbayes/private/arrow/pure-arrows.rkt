#lang typed/racket/base

(require racket/promise
         racket/match
         "../set.rkt"
         "../flonum.rkt"
         "parameters.rkt"
         "types.rkt"
         "preimage-mapping.rkt"
         "cache.rkt")

(provide (all-defined-out))

(: exact-pre-mapping-fun (-> (-> Nonempty-Set Set) (-> Nonempty-Set (Values Set #t))))
(define ((exact-pre-mapping-fun f) B)
  (values (f B) #t))

(: run/bot (case-> (Bot-Arrow Bottom -> Bottom)
                   (Bot-Arrow Maybe-Value -> Maybe-Value)))
(define (run/bot f a)
  (if (bottom? a) a (f a)))

(: run/pre (case-> (-> Pre-Arrow Empty-Set Empty-Pre-Mapping)
                   (-> Pre-Arrow Set Pre-Mapping)
                   (-> Pre-Arrow Empty-Set Boolean Empty-Pre-Mapping)
                   (-> Pre-Arrow Set Boolean Pre-Mapping)))
(define (run/pre h A [exact? #t])
  (if (empty-set? A)
      empty-pre-mapping
      (if exact?
          (h A)
          (let ([h  (h A)])
            (cond [(empty-pre-mapping? h)  empty-pre-mapping]
                  [else  (match-define (nonempty-pre-mapping Y p) h)
                         (nonempty-pre-mapping
                          Y (λ (B)
                              (define-values (A _) (p B))
                              (values A #f)))])))))

;; ===================================================================================================
;; Basic computable lifts

;; ---------------------------------------------------------------------------------------------------
;; Failure

(define fail-value (bottom (delay "fail")))

(: fail/bot (-> Bot-Arrow))
(: fail/pre (-> Pre-Arrow))

(define ((fail/bot) a) fail-value)
(define ((fail/pre) A) empty-pre-mapping)

;; ---------------------------------------------------------------------------------------------------
;; Identity function

(: id/bot (-> Bot-Arrow))
(: id/pre (-> Pre-Arrow))

(define ((id/bot) a) a)
(define (id/pre) (make-pre-arrow/memo (λ (A) (nonempty-pre-mapping A (λ (B) (values B #t))))))

;; ---------------------------------------------------------------------------------------------------
;; Domain restriction

(: restrict/bot (-> Nonempty-Set Bot-Arrow))
(: restrict/pre (-> Nonempty-Set Pre-Arrow))

(define ((restrict/bot X) a)
  (if (set-member? X a) a (bottom (delay (format "restrict: expected value in ~e; given ~e" X a)))))

(define (restrict/pre X)
  (make-pre-arrow/memo (λ (A) (pre-mapping (set-intersect A X) (λ (B) (values B #t))))))

;; ---------------------------------------------------------------------------------------------------
;; Constant functions

(: const/bot (-> Value Bot-Arrow))
(: const/pre (-> Value Pre-Arrow))

(define ((const/bot b) a) b)
(define (const/pre b)
  (define B (value->singleton b))
  (make-pre-arrow/memo (λ (A) (nonempty-pre-mapping B (λ (B) (values A #t))))))

;; ---------------------------------------------------------------------------------------------------
;; Pair projections

(: fst/bot (-> Bot-Arrow))
(: snd/bot (-> Bot-Arrow))
(: fst/pre (-> Pre-Arrow))
(: snd/pre (-> Pre-Arrow))

(define (fst/bot) value-fst)
(define (snd/bot) value-snd)
(define (fst/pre) (make-pre-arrow/memo (λ (A) (pre-mapping
                                               (set-fst A)
                                               (exact-pre-mapping-fun (set-unfst A))))))
(define (snd/pre) (make-pre-arrow/memo (λ (A) (pre-mapping
                                               (set-snd A)
                                               (exact-pre-mapping-fun (set-unsnd A))))))

;; ---------------------------------------------------------------------------------------------------
;; List projections

(: list-ref/bot (-> Natural Bot-Arrow))
(: list-ref/pre (-> Natural Pre-Arrow))

(define ((list-ref/bot n) a) (value-list-ref a n))
(define (list-ref/pre n) (make-pre-arrow/memo (λ (A) (pre-mapping
                                                      (set-proj A n)
                                                      (exact-pre-mapping-fun (set-unproj A n))))))

;; ===================================================================================================
;; Arrow combinators (except the uncomputable `arr')

;; ---------------------------------------------------------------------------------------------------
;; Composition

(: >>>/bot (-> Bot-Arrow Bot-Arrow Bot-Arrow))
(: >>>/pre (-> Pre-Arrow Pre-Arrow Pre-Arrow))

(define ((>>>/bot f1 f2) a)
  (run/bot f2 (f1 a)))

(define (>>>/pre h1 h2)
  (define compose/pre (make-compose/pre))
  (make-pre-arrow/memo
   (λ (A)
     (let* ([h1  (run/pre h1 A)]
            [h2  (run/pre h2 (range/pre h1))])
       (compose/pre h2 h1)))))

;; ---------------------------------------------------------------------------------------------------
;; Pairing

(: &&&/bot (-> Bot-Arrow Bot-Arrow Bot-Arrow))
(: &&&/pre (-> Pre-Arrow Pre-Arrow Pre-Arrow))

(define ((&&&/bot f1 f2) a)
  (define b1 (f1 a))
  (cond [(bottom? b1)  b1]
        [else  (define b2 (f2 a))
               (cond [(bottom? b2)  b2]
                     [else  (cons b1 b2)])]))

(define (&&&/pre h1 h2)
  (define pair/pre (make-pair/pre))
  (make-pre-arrow/memo
   (λ (A)
     (let ([h1  (run/pre h1 A)]
           [h2  (run/pre h2 A)])
       (pair/pre h1 h2)))))

;; ---------------------------------------------------------------------------------------------------
;; Strict if-then-else

(: ifte/bot (-> Bot-Arrow Bot-Arrow Bot-Arrow Bot-Arrow))
(: ifte/pre (-> Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow))

(define ((ifte/bot f1 f2 f3) a)
  (define b (f1 a))
  (cond [(bottom? b)  b]
        [(eq? b #t)  (f2 a)]
        [(eq? b #f)  (f3 a)]
        [else  (bottom (delay (format "ifte/bot: expected Boolean condition; given ~e" b)))]))

(define (ifte/pre h1 h2 h3)
  (define uplus/pre (make-uplus/pre))
  (make-pre-arrow/memo
   (λ (A)
     (let*-values ([(h1)  (run/pre h1 A)]
                   [(At At-exact?)  (preimage/pre h1 trues)]
                   [(Af Af-exact?)  (preimage/pre h1 falses)]
                   [(h2)  (run/pre h2 At)]
                   [(h3)  (run/pre h3 Af)])
       (uplus/pre At-exact? Af-exact? h2 h3)))))

;; ---------------------------------------------------------------------------------------------------
;; Laziness

(: lazy/bot (-> (Promise Bot-Arrow) Bot-Arrow))
(: lazy/pre (-> (Promise Pre-Arrow) Pre-Arrow))

(define ((lazy/bot f) a) ((force f) a))
(define ((lazy/pre h) A) (run/pre (force h) A))

;; ---------------------------------------------------------------------------------------------------
;; Extra combinators needed for store splitting in the Bottom* and Preimage* arrows

(: first/bot (-> Bot-Arrow Bot-Arrow))
(: first/pre (-> Pre-Arrow Pre-Arrow))

(define (first/bot f) (&&&/bot (>>>/bot (fst/bot) f) (snd/bot)))
(define (first/pre h) (&&&/pre (>>>/pre (fst/pre) h) (snd/pre)))

;; ===================================================================================================
;; Total if-then-else

(: ifte*/bot (-> Bot-Arrow Bot-Arrow Bot-Arrow Bot-Arrow Bot-Arrow))
(define ((ifte*/bot fb f1 f2 f3) a)
  (define b* (fb a))
  (define b (f1 a))
  (cond [(not (eq? b* b))  (bottom (delay (format "ifte*: expected ~a condition; got ~e" b* b)))]
        [(eq? b #t)  (f2 a)]
        [(eq? b #f)  (f3 a)]
        [else  (bottom (delay (format "ifte*: expected boolean condition; got ~e" b)))]))

(: terminating-ifte*/pre (-> Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow))
;; This direct implementation ensures termination
(define (terminating-ifte*/pre hb h1 h2 h3)
  (make-pre-arrow/memo
   (λ (A)
     (let ([hb  (run/pre hb A)]
           [h1  (run/pre h1 A)])
       (cond [(or (empty-pre-mapping? h1) (empty-pre-mapping? hb))  empty-pre-mapping]
             [else
              (match-define (nonempty-pre-mapping C1 p1) h1)
              (match-define (nonempty-pre-mapping Cb pb) hb)
              (define C (set-intersect C1 Cb))
              (define Ct (set-intersect C trues))
              (define Cf (set-intersect C falses))
              (define-values (A2 A2-exact?)
                (if (empty-set? Ct)
                    (values empty-set #t)
                    (let-values ([(A A-exact?)  (p1 Ct)]
                                 [(B B-exact?)  (pb Ct)])
                      (values (set-intersect A B) (and A-exact? B-exact?)))))
              (define-values (A3 A3-exact?)
                (if (empty-set? Cf)
                    (values empty-set #t)
                    (let-values ([(A A-exact?)  (p1 Cf)]
                                 [(B B-exact?)  (pb Cf)])
                      (values (set-intersect A B) (and A-exact? B-exact?)))))
              (cond [(eq? Cb bools)  (define-values (A A-exact?) (set-join A2 A3))
                                     (nonempty-pre-mapping universe (λ (B) (values A #f)))]
                    [(eq? Cb trues)   (run/pre h2 A2 A2-exact?)]
                    [(eq? Cb falses)  (run/pre h3 A3 A3-exact?)]
                    [else  empty-pre-mapping])])))))

(: precise-ifte*/pre (-> Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow))
;; This more precise ifte* combinator can rule out branches using preimages computed by h1, but
;; doesn't ensure termination on some programs
(define (precise-ifte*/pre hb h1 h2 h3)
  (make-pre-arrow/memo
   (λ (A)
     ;; Compute a preimage functon for the condition restricted to A
     (let ([h1  (run/pre h1 A)])
       ;; Compute the preimages of {true} and {false} restricted to A
       (define-values (A2 A2-exact?) (preimage/pre h1 trues))
       (define-values (A3 A3-exact?) (preimage/pre h1 falses))
       ;; Compute preimage functions for the branch restricted to A2 and A3
       (define hb2 (run/pre hb A2 A2-exact?))
       (define hb3 (run/pre hb A3 A3-exact?))
       (let*-values ([(At A2-exact?)  (preimage/pre hb2 trues)]
                     [(A2)  (set-intersect A2 At)]
                     [(Af A3-exact?)  (preimage/pre hb3 falses)]
                     [(A3)  (set-intersect A3 Af)])
         (cond [(and (empty-set? A2) (empty-set? A3))  empty-pre-mapping]
               [(empty-set? A3)  (run/pre h2 A2 A2-exact?)]
               [(empty-set? A2)  (run/pre h3 A3 A3-exact?)]
               [else  (define-values (A A-exact?) (set-join A2 A3))
                      (nonempty-pre-mapping universe (λ (B) (values A #f)))]))))))

(: ifte*/pre (-> Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow))
(define (ifte*/pre hb h1 h2 h3)
  (if (drbayes-always-terminate?)
      (terminating-ifte*/pre hb h1 h2 h3)
      (precise-ifte*/pre hb h1 h2 h3)))

;; ===================================================================================================
;; Store projection and splitting

(: store-uniform/bot (-> (-> Value (U Bottom Prob))))
(: store-branch/bot (-> Bot-Arrow))
(: store-left/bot   (-> Bot-Arrow))
(: store-right/bot  (-> Bot-Arrow))

(: store-uniform/pre (-> Pre-Arrow))
(: store-branch/pre (-> Pre-Arrow))
(: store-left/pre   (-> Pre-Arrow))
(: store-right/pre  (-> Pre-Arrow))

(define ((store-uniform/bot) a)
  (cond [(store? a)
         (define x (store-random a))
         (cond [(prob? x)  x]
               [else  (bottom (delay (format "store-uniform/bot: zero-probability axis")))])]
        [else  (bottom (delay (format "store-uniform/bot: expected Store; given ~e" a)))]))

(void (ann store-uniform/bot (-> Bot-Arrow)))

(define ((store-branch/bot) a)
  (cond [(store? a)  (store-branch a)]
        [else  (bottom (delay (format "store-uniform/bot: expected Store; given ~e" a)))]))

(define ((store-left/bot) a)
  (cond [(store? a)  (store-left a)]
        [else  (bottom (delay (format "store-left/bot: expected Store; given ~e" a)))]))

(define ((store-right/bot) a)
  (cond [(store? a)  (store-right a)]
        [else  (bottom (delay (format "store-right/bot: expected Store; given ~e" a)))]))

(define (store-uniform/pre)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (S)
     (let ([S  (set-take-stores S)])
       (if (empty-store-set? S)
           empty-pre-mapping
           (nonempty-pre-mapping
            (store-set-random S)
            (fun (λ (X) (let ([S  (store-set-unrandom S (set-take-probs X))])
                          (values (if (empty-store-set? S) empty-set S) #t))))))))))

(define (store-branch/pre)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (S)
     (let ([S  (set-take-stores S)])
       (if (empty-store-set? S)
           empty-pre-mapping
           (nonempty-pre-mapping
            (store-set-branch S)
            (fun (λ (B) (let ([S  (store-set-unbranch S (set-take-bools B))])
                          (values (if (empty-store-set? S) empty-set S) #t))))))))))

(define (store-left/pre)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (S)
     (let ([S  (set-take-stores S)])
       (if (empty-store-set? S)
           empty-pre-mapping
           (nonempty-pre-mapping
            (store-set-left S)
            (fun (λ (L) (let ([S  (store-set-unleft S (set-take-stores L))])
                          (values (if (empty-store-set? S) empty-set S) #t))))))))))

(define (store-right/pre)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (S)
     (let ([S  (set-take-stores S)])
       (if (empty-store-set? S)
           empty-pre-mapping
           (nonempty-pre-mapping
            (store-set-right S)
            (fun (λ (R) (let ([S  (store-set-unright S (set-take-stores R))])
                          (values (if (empty-store-set? S) empty-set S) #t))))))))))

;; ---------------------------------------------------------------------------------------------------
;; Random boolean store projection

(: store-boolean/bot (-> Flonum Bot-Arrow))
(define (store-boolean/bot p)
  (cond [(and (p . > . 0.0) (p . < . 1.0))
         (define uniform/bot (store-uniform/bot))
         (let ([p  (assert (flonum->prob p) prob?)])
           (λ (a)
             (define x (uniform/bot a))
             (if (bottom? x) x (prob< x p))))]
        [else
         (const/bot (p . >= . 1.0))]))

(: boolean-preimage (-> Nonnegative-Flonum (Values Plain-Prob-Interval
                                                   Plain-Prob-Interval)))
(define (boolean-preimage orig-p)
  (define p (flonum->prob orig-p))
  (cond [(prob? p)
         (values (plain-prob-interval prob-0 p #t #f)
                 (plain-prob-interval p prob-1 #t #t))]
        [else
         (raise-argument-error 'boolean-preimage "Flonum in (0,1)" orig-p)]))

(: store-boolean/pre (-> Flonum Pre-Arrow))
(define (store-boolean/pre p)
  (cond [(and (p . > . 0.0) (p . < . 1.0))
         (define fun (make-pre-mapping-fun/memo))
         (define-values (Xt Xf) (boolean-preimage p))
         (make-pre-arrow/memo
          (λ (S)
            (let ([S  (set-take-stores S)])
              (if (empty-store-set? S)
                  empty-pre-mapping
                  (let* ([X  (store-set-random S)]
                         [Xt  (prob-set-intersect Xt X)]
                         [Xf  (prob-set-intersect Xf X)])
                    (cond [(and (empty-prob-set? Xt) (empty-prob-set? Xf))
                           empty-pre-mapping]
                          [(empty-prob-set? Xf)
                           (nonempty-pre-mapping
                            trues  (fun (λ (B) (let ([S  (store-set-unrandom S Xt)])
                                                 (values (if (empty-store-set? S) empty-set S)
                                                         #t)))))]
                          [(empty-prob-set? Xt)
                           (nonempty-pre-mapping
                            falses (fun (λ (B) (let ([S  (store-set-unrandom S Xf)])
                                                 (values (if (empty-store-set? S) empty-set S)
                                                         #t)))))]
                          [else
                           (nonempty-pre-mapping
                            bools  (fun (λ (B) (let* ([X  (cond [(eq? B trues)   Xt]
                                                                [(eq? B falses)  Xf]
                                                                [else  (prob-set-union Xt Xf)])]
                                                      [S  (store-set-unrandom S X)])
                                                 (values (if (empty-store-set? S) empty-set S)
                                                         #t)))))]))))))]
        [else
         (const/pre (p . >= . 1.0))]))
