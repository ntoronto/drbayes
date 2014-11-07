#lang typed/racket/base

(require racket/promise
         racket/match
         "../set.rkt"
         "preimage-mapping.rkt")

(provide (all-defined-out))

(: drbayes-always-terminate? (Parameterof Boolean))
(define drbayes-always-terminate? (make-parameter #f))

(define-type Bot-Arrow (Value -> Maybe-Value))
(define-type Pre-Arrow (Nonempty-Set -> Pre-Mapping))

(: run/bot (case-> (Bot-Arrow Bottom -> Bottom)
                   (Bot-Arrow Maybe-Value -> Maybe-Value)))
(define (run/bot f a)
  (if (bottom? a) a (f a)))

(: run/pre (case-> (Pre-Arrow Empty-Set -> Empty-Pre-Mapping)
                   (Pre-Arrow Set -> Pre-Mapping)))
(define (run/pre h A)
  (if (empty-set? A) empty-pre-mapping (h A)))

;; ===================================================================================================
;; Basic computable lifts

;; ---------------------------------------------------------------------------------------------------
;; Failure

(define fail-value (bottom (delay "fail")))

(: fail/bot Bot-Arrow)
(define (fail/bot a) fail-value)

(: fail/pre Pre-Arrow)
(define (fail/pre A) empty-pre-mapping)

;; ---------------------------------------------------------------------------------------------------
;; Identity function

(: id/bot Bot-Arrow)
(define (id/bot a) a)

(: id/pre Pre-Arrow)
(define (id/pre A) (nonempty-pre-mapping A (λ (B) B)))

;; ---------------------------------------------------------------------------------------------------
;; Domain restriction

(: restrict/bot (Nonempty-Set -> Bot-Arrow))
(define ((restrict/bot X) a)
  (cond [(set-member? X a)  a]
        [else  (bottom (delay (format "restrict: expected value in ~e; given ~e" X a)))]))

(: restrict/pre (Nonempty-Set -> Pre-Arrow))
(define ((restrict/pre X) A)
  (let ([A  (set-intersect A X)])
    (cond [(empty-set? A)  empty-pre-mapping]
          [else  (nonempty-pre-mapping A (λ (B) B))])))

;; ---------------------------------------------------------------------------------------------------
;; Constant functions

(: const/bot (Value -> Bot-Arrow))
(define ((const/bot b) a) b)

(: const/pre (Value -> Pre-Arrow))
(define (const/pre b)
  (define B (value->singleton b))
  (λ (A) (nonempty-pre-mapping B (λ (B) A))))

;; ---------------------------------------------------------------------------------------------------
;; Pair and list projections

(: ref/bot (Pair-Index -> Bot-Arrow))
(define ((ref/bot n) a) (value-pair-ref a n))

(: ref/pre (Pair-Index -> Pre-Arrow))
(define ((ref/pre n) A) (pre-mapping (set-proj A n) (λ (B) (set-unproj A n B))))

;; ===================================================================================================
;; Arrow combinators (except the uncomputable `arr')

;; ---------------------------------------------------------------------------------------------------
;; Composition

(: >>>/bot (Bot-Arrow Bot-Arrow -> Bot-Arrow))
(define ((>>>/bot f1 f2) a)
  (run/bot f2 (f1 a)))

(: >>>/pre (Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((>>>/pre h1 h2) A)
  (let* ([h1  (h1 A)]
         [h2  (run/pre h2 (range/pre h1))])
    (compose/pre h2 h1)))

;; ---------------------------------------------------------------------------------------------------
;; Pairing

(: &&&/bot (Bot-Arrow Bot-Arrow -> Bot-Arrow))
(define ((&&&/bot f1 f2) a)
  (define b1 (f1 a))
  (cond [(bottom? b1)  b1]
        [else  (define b2 (f2 a))
               (cond [(bottom? b2)  b2]
                     [else  (cons b1 b2)])]))

(: &&&/pre (Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((&&&/pre h1 h2) A)
  (let ([h1  (h1 A)]
        [h2  (h2 A)])
    (pair/pre h1 h2)))

;; ---------------------------------------------------------------------------------------------------
;; Partial if-then-else

(: ifte/bot (Bot-Arrow Bot-Arrow Bot-Arrow -> Bot-Arrow))
(define ((ifte/bot f1 f2 f3) a)
  (define b (f1 a))
  (cond [(bottom? b)  b]
        [(eq? b #t)  (f2 a)]
        [(eq? b #f)  (f3 a)]
        [else  (bottom (delay (format "ifte/bot: expected Boolean condition; given ~e" b)))]))

(: ifte/pre (Pre-Arrow Pre-Arrow Pre-Arrow -> Pre-Arrow))
(define ((ifte/pre h1 h2 h3) A)
  (let* ([h1  (h1 A)]
         [h2  (run/pre h2 (ap/pre h1 trues))]
         [h3  (run/pre h3 (ap/pre h1 falses))])
    (uplus/pre h2 h3)))

;; ---------------------------------------------------------------------------------------------------
;; Laziness

(: lazy/bot ((Promise Bot-Arrow) -> Bot-Arrow))
(define ((lazy/bot f) a) ((force f) a))

(: lazy/pre ((Promise Pre-Arrow) -> Pre-Arrow))
(define ((lazy/pre h) A) ((force h) A))

;; ---------------------------------------------------------------------------------------------------
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
;; This direct translation from the paper ensures termination
(define ((terminating-ifte*/pre hb h1 h2 h3) A)
  (let ([hb  (hb A)]
        [h1  (h1 A)])
    (cond [(or (empty-pre-mapping? h1) (empty-pre-mapping? hb))  empty-pre-mapping]
          [else
           (match-define (nonempty-pre-mapping C1 p1) h1)
           (match-define (nonempty-pre-mapping Cb pb) hb)
           (define C (set-intersect C1 Cb))
           (define Ct (set-intersect C trues))
           (define Cf (set-intersect C falses))
           (define A2 (if (empty-set? Ct) empty-set (set-intersect (p1 Ct) (pb Ct))))
           (define A3 (if (empty-set? Cf) empty-set (set-intersect (p1 Cf) (pb Cf))))
           (cond [(eq? Cb bools)  (define A (set-join A2 A3))
                                  (nonempty-pre-mapping universe (λ (B) A))]
                 [(eq? Cb trues)   (run/pre h2 A2)]
                 [(eq? Cb falses)  (run/pre h3 A3)]
                 [else  empty-pre-mapping])])))

(: precise-ifte*/pre (-> Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow))
;; A more precise ifte* combinator that can rule out branches using preimages computed by h1, but
;; doesn't always ensure termination
;; Conjecture: if a program's interpretation as a bot* arrow terminates with probability 1, a pre*
;; arrow interpretation that uses this approximation also terminates with probability 1
(define ((precise-ifte*/pre hb h1 h2 h3) A)
  (let ([h1  (h1 A)])
    (define A2 (ap/pre h1 trues))
    (define A3 (ap/pre h1 falses))
    (define hb2 (run/pre hb A2))
    (define hb3 (run/pre hb A3))
    (let ([A2  (set-intersect A2 (ap/pre hb2 trues))]
          [A3  (set-intersect A3 (ap/pre hb3 falses))])
      (cond [(and (empty-set? A2) (empty-set? A3))  empty-pre-mapping]
            [(empty-set? A3)  (h2 A2)]
            [(empty-set? A2)  (h3 A3)]
            [else  (define A (set-join A2 A3))
                   (nonempty-pre-mapping universe (λ (B) A))]))))

(: ifte*/pre (-> Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow Pre-Arrow))
(define (ifte*/pre hb h1 h2 h3)
  (if (drbayes-always-terminate?)
      (terminating-ifte*/pre hb h1 h2 h3)
      (precise-ifte*/pre hb h1 h2 h3)))

;; ===================================================================================================
;; Store projection and splitting

;; ---------------------------------------------------------------------------------------------------
;; Omega

(: omega-value/bot Bot-Arrow)
(define (omega-value/bot a)
  (if (omega? a)
      (omega-value a)
      (bottom (delay (format "omega-value/bot: expected Omega; given ~e" a)))))

(: omega-left/bot Bot-Arrow)
(define (omega-left/bot a)
  (if (omega? a)
      (omega-left a)
      (bottom (delay (format "omega-left/bot: expected Omega; given ~e" a)))))

(: omega-right/bot Bot-Arrow)
(define (omega-right/bot a)
  (if (omega? a)
      (omega-right a)
      (bottom (delay (format "omega-right/bot: expected Omega; given ~e" a)))))

(: omega-value/pre Pre-Arrow)
(define (omega-value/pre Ω)
  (let ([Ω  (set-take-omegas Ω)])
    (pre-mapping (bot-basic (omega-set-axis Ω))
                 (λ (Ω1) (bot-basic (omega-set-unaxis Ω (set-take-reals Ω1)))))))

(: omega-left/pre Pre-Arrow)
(define (omega-left/pre Ω)
  (let ([Ω  (set-take-omegas Ω)])
    (pre-mapping (bot-basic (omega-set-left Ω))
                 (λ (Ω1) (bot-basic (omega-set-unleft Ω (set-take-omegas Ω1)))))))

(: omega-right/pre Pre-Arrow)
(define (omega-right/pre Ω)
  (let ([Ω  (set-take-omegas Ω)])
    (pre-mapping (bot-basic (omega-set-right Ω))
                 (λ (Ω1) (bot-basic (omega-set-unright Ω (set-take-omegas Ω1)))))))

;; ---------------------------------------------------------------------------------------------------
;; Traces

(: trace-value/bot Bot-Arrow)
(define (trace-value/bot a)
  (if (trace? a)
      (trace-value a)
      (bottom (delay (format "trace-value/bot: expected Omega; given ~e" a)))))

(: trace-left/bot Bot-Arrow)
(define (trace-left/bot a)
  (if (trace? a)
      (trace-left a)
      (bottom (delay (format "trace-left/bot: expected Omega; given ~e" a)))))

(: trace-right/bot Bot-Arrow)
(define (trace-right/bot a)
  (if (trace? a)
      (trace-right a)
      (bottom (delay (format "trace-right/bot: expected Omega; given ~e" a)))))

(: trace-value/pre Pre-Arrow)
(define (trace-value/pre T)
  (let ([T  (set-take-traces T)])
    (pre-mapping (bot-basic (trace-set-axis T))
                 (λ (T1) (bot-basic (trace-set-unaxis T (set-take-bools T1)))))))

(: trace-left/pre Pre-Arrow)
(define (trace-left/pre T)
  (let ([T  (set-take-traces T)])
    (pre-mapping (bot-basic (trace-set-left T))
                 (λ (T1) (bot-basic (trace-set-unleft T (set-take-traces T1)))))))

(: trace-right/pre Pre-Arrow)
(define (trace-right/pre T)
  (let ([T  (set-take-traces T)])
    (pre-mapping (bot-basic (trace-set-right T))
                 (λ (T1) (bot-basic (trace-set-unright T (set-take-traces T1)))))))

;; ---------------------------------------------------------------------------------------------------
;; Pairs of omega and trace

(: par/bot (-> Bot-Arrow Bot-Arrow Bot-Arrow))
(define (par/bot f1 f2)
  (&&&/bot (>>>/bot (ref/bot 'fst) f1)
           (>>>/bot (ref/bot 'snd) f2)))

(: par/pre (-> Pre-Arrow Pre-Arrow Pre-Arrow))
(define (par/pre h1 h2)
  (&&&/pre (>>>/pre (ref/pre 'fst) h1)
           (>>>/pre (ref/pre 'snd) h2)))

(define store-left/bot (par/bot omega-left/bot trace-left/bot))
(define store-right/bot (par/bot omega-right/bot trace-right/bot))

(define store-left/pre (par/pre omega-left/pre trace-left/pre))
(define store-right/pre (par/pre omega-right/pre trace-right/pre))

;; ===================================================================================================

(: first/bot (-> Bot-Arrow Bot-Arrow))
(define (first/bot f)
  (&&&/bot (>>>/bot (ref/bot 'fst) f) (ref/bot 'snd)))

(: first/pre (-> Pre-Arrow Pre-Arrow))
(define (first/pre h)
  (&&&/pre (>>>/pre (ref/pre 'fst) h) (ref/pre 'snd)))
