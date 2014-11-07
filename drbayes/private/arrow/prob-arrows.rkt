#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "../set.rkt"
         "../untyped-utils.rkt"
         "indexes.rkt"
         "preimage-mapping.rkt"
         "pure-arrows.rkt"
         "pure-lifts.rkt")

(provide (all-defined-out))

(struct: bot-wrapper ([arrow : Bot-Arrow]) #:transparent)
(struct: bot*-arrow ([arrow : Bot-Arrow]) #:transparent)

(struct: pre-wrapper ([arrow : Pre-Arrow]) #:transparent)
(struct: pre*-arrow ([arrow : Pre-Arrow]) #:transparent)

(define-type Bot*-Arrow (U bot-wrapper bot*-arrow))
(define-type Pre*-Arrow (U pre-wrapper pre*-arrow))
(define-type Idx-Arrow (Tree-Index -> Indexes))

(: η/bot* (Bot-Arrow -> Bot*-Arrow))
(define η/bot* bot-wrapper)

(: η/pre* (Pre-Arrow -> Pre*-Arrow))
(define η/pre* pre-wrapper)

(: run/bot* (Bot*-Arrow -> Bot-Arrow))
(define (run/bot* k)
  (if (bot-wrapper? k)
      ((ref/bot 'snd) . >>>/bot . (bot-wrapper-arrow k))
      (bot*-arrow-arrow k)))

(: run/pre* (Pre*-Arrow -> Pre-Arrow))
(define (run/pre* k)
  (if (pre-wrapper? k)
      ((ref/pre 'snd) . >>>/pre . (pre-wrapper-arrow k))
      (pre*-arrow-arrow k)))

(: any/idx Idx-Arrow)
(define (any/idx j) '())

;; ===================================================================================================
;; Basic computable lifts

(define fail/bot* (η/bot* fail/bot))
(define fail/pre* (η/pre* fail/pre))
(define fail/idx any/idx)

(define id/bot* (η/bot* id/bot))
(define id/pre* (η/pre* id/pre))
(define id/idx any/idx)

(define restrict/bot* (λ: ([X : Nonempty-Set]) (η/bot* (restrict/bot X))))
(define restrict/pre* (λ: ([X : Nonempty-Set]) (η/pre* (restrict/pre X))))
(define restrict/idx (λ: ([X : Nonempty-Set]) any/idx))

(define const/bot* (λ: ([b : Value]) (η/bot* (const/bot b))))
(define const/pre* (λ: ([b : Value]) (η/pre* (const/pre b))))
(define const/idx (λ: ([b : Value]) any/idx))

(define ref/bot* (λ: ([j : Pair-Index]) (η/bot* (ref/bot j))))
(define ref/pre* (λ: ([j : Pair-Index]) (η/pre* (ref/pre j))))
(define ref/idx (λ: ([j : Pair-Index]) any/idx))

;; ===================================================================================================
;; Arrow combinators

;; ---------------------------------------------------------------------------------------------------
;; Composition

(: >>>/bot* (Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (k1 . >>>/bot* . k2)
  (cond [(and (bot-wrapper? k1) (bot-wrapper? k2))
         (bot-wrapper ((bot-wrapper-arrow k1) . >>>/bot . (bot-wrapper-arrow k2)))]
        [else
         (bot*-arrow
          (>>>/bot (&&&/bot (>>>/bot (ref/bot 'fst) store-right/bot)
                            (>>>/bot (first/bot store-left/bot) (run/bot* k1)))
                   (run/bot* k2)))]))

(: >>>/pre* (Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (k1 . >>>/pre* . k2)
  (cond [(and (pre-wrapper? k1) (pre-wrapper? k2))
         (pre-wrapper ((pre-wrapper-arrow k1) . >>>/pre . (pre-wrapper-arrow k2)))]
        [else
         (pre*-arrow
          (>>>/pre (&&&/pre (>>>/pre (ref/pre 'fst) store-right/pre)
                            (>>>/pre (first/pre store-left/pre) (run/pre* k1)))
                   (run/pre* k2)))]))

(: >>>/idx (Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((>>>/idx k1 k2) j)
  (append (k1 (left j)) (k2 (right j))))

;; ---------------------------------------------------------------------------------------------------
;; Pairing

(: &&&/bot* (Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (k1 . &&&/bot* . k2)
  (cond [(and (bot-wrapper? k1) (bot-wrapper? k2))
         (bot-wrapper ((bot-wrapper-arrow k1) . &&&/bot . (bot-wrapper-arrow k2)))]
        [else
         (bot*-arrow
          (&&&/bot (>>>/bot (first/bot store-left/bot) (run/bot* k1))
                   (>>>/bot (first/bot store-right/bot) (run/bot* k2))))]))

(: &&&/pre* (Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (k1 . &&&/pre* . k2)
  (cond [(and (pre-wrapper? k1) (pre-wrapper? k2))
         (pre-wrapper ((pre-wrapper-arrow k1) . &&&/pre . (pre-wrapper-arrow k2)))]
        [else
         (pre*-arrow
          (&&&/pre (>>>/pre (first/pre store-left/pre) (run/pre* k1))
                   (>>>/pre (first/pre store-right/pre) (run/pre* k2))))]))

(: &&&/idx (Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((&&&/idx k1 k2) j)
  (append (k1 (left j)) (k2 (right j))))

;; ---------------------------------------------------------------------------------------------------
;; Partial if-then-else

(: ifte/bot* (Bot*-Arrow Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (ifte/bot* k1 k2 k3)
  (cond [(and (bot-wrapper? k1) (bot-wrapper? k2) (bot-wrapper? k3))
         (bot-wrapper (ifte/bot (bot-wrapper-arrow k1)
                                (bot-wrapper-arrow k2)
                                (bot-wrapper-arrow k3)))]
        [else
         (bot*-arrow
          (ifte/bot (>>>/bot (first/bot store-left/bot) (run/bot* k1))
                    (>>>/bot (first/bot (>>>/bot store-right/bot store-left/bot)) (run/bot* k2))
                    (>>>/bot (first/bot (>>>/bot store-right/bot store-right/bot)) (run/bot* k3))))]))

(: ifte/pre* (Pre*-Arrow Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (ifte/pre* k1 k2 k3)
  (cond [(and (pre-wrapper? k1) (pre-wrapper? k2) (pre-wrapper? k3))
         (pre-wrapper (ifte/pre (pre-wrapper-arrow k1)
                                (pre-wrapper-arrow k2)
                                (pre-wrapper-arrow k3)))]
        [else
         (pre*-arrow
          (ifte/pre (>>>/pre (first/pre store-left/pre) (run/pre* k1))
                    (>>>/pre (first/pre (>>>/pre store-right/pre store-left/pre)) (run/pre* k2))
                    (>>>/pre (first/pre (>>>/pre store-right/pre store-right/pre)) (run/pre* k3))))]))

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

(: branch/bot* Bot*-Arrow)
(define branch/bot*
  (bot*-arrow
   (λ: ([a : Value])
     (match a
       [(cons (cons (? omega?) (? trace? t)) _)  (trace-value t)]
       [_  (proj-domain-fail 'branch/bot* a)]))))

(: branch/pre Pre-Arrow)
(define (branch/pre A)
  (define T (set-take-traces A))
  (cond [(empty-set? T)  empty-pre-mapping]
        [else  (define B (trace-set-axis T))
               (cond [(empty-bool-set? B)  empty-pre-mapping]
                     [else  (nonempty-pre-mapping
                             B (λ (B) (let ([T  (trace-set-unaxis T (set-take-bools B))])
                                        (cond [(empty-trace-set? T)  empty-set]
                                              [else  T]))))])]))

(: branch/pre* Pre*-Arrow)
(define branch/pre*
  (pre*-arrow
   ((ref/pre 'fst) . >>>/pre . ((ref/pre 'snd) . >>>/pre . branch/pre))))

;; ---------------------------------------------------------------------------------------------------
;; Random source projections

(: random/bot* Bot*-Arrow)
(define random/bot*
  (bot*-arrow
   (λ: ([a : Value])
     (match a
       [(cons (cons (? omega? r) (? trace?)) _)  (omega-value r)]
       [_  (proj-domain-fail 'random/bot* a)]))))

(: random/pre Pre-Arrow)
(define (random/pre A)
  (define R (set-take-omegas A))
  (cond [(empty-set? R)  empty-pre-mapping]
        [else  (define B (real-set-intersect unit-interval (omega-set-axis R)))
               (cond [(empty-real-set? B)  empty-pre-mapping]
                     [else  (nonempty-pre-mapping
                             B (λ (B) (let ([R  (omega-set-unaxis R (real-set-intersect
                                                                     unit-interval
                                                                     (set-take-reals B)))])
                                        (cond [(empty-omega-set? R)  empty-set]
                                              [else  R]))))])]))

(: random/pre* Pre*-Arrow)
(define random/pre*
  (pre*-arrow
   ((ref/pre 'fst) . >>>/pre . ((ref/pre 'fst) . >>>/pre . random/pre))))

(: random/idx Idx-Arrow)
(define (random/idx j)
  (list (random-index j #f)))

;; ---------------------------------------------------------------------------------------------------
;; Random source boolean projections

(: boolean/bot* (Flonum -> Bot*-Arrow))
(define (boolean/bot* p)
  (define random (run/bot* random/bot*))
  (bot*-arrow
   (λ: ([a : Value])
     (let ([b  (random a)])
       (and (flonum? b) (b . < . p))))))

(: boolean-preimage (Flonum -> (Values Nonextremal-Interval Nonextremal-Interval)))
;; Assumes p > 0.0 and p < 1.0
(define (boolean-preimage p)
  (values (Nonextremal-Interval 0.0 p #t #f)
          (Nonextremal-Interval p 1.0 #t #t)))

(: boolean/pre (Flonum -> Pre-Arrow))
(define (boolean/pre p)
  (cond [(and (p . > . 0.0) (p . < . 1.0))
         (define-values (It If) (boolean-preimage p))
         (λ (A)
           (define R (set-take-omegas A))
           (define Rj (real-set-intersect unit-interval (omega-set-axis R)))
           (let ([It  (real-set-intersect It Rj)]
                 [If  (real-set-intersect If Rj)])
             (define-values (B I)
               (cond [(and (empty-real-set? It) (empty-real-set? If))
                      (values empty-set empty-real-set)]
                     [(empty-real-set? If)  (values trues It)]
                     [(empty-real-set? It)  (values falses It)]
                     [else  (values bools (real-set-union It If))]))
             (pre-mapping B (λ (B)
                              (let ([R  (cond [(eq? B trues)   (omega-set-unaxis R It)]
                                              [(eq? B falses)  (omega-set-unaxis R If)]
                                              [else  (omega-set-unaxis R I)])])
                                (if (empty-omega-set? R) empty-set R))))))]
        [else
         (const/pre (p . >= . 1.0))]))

(: boolean/pre* (Flonum -> Pre*-Arrow))
(define (boolean/pre* p)
  (pre*-arrow
   ((ref/pre 'fst) . >>>/pre . ((ref/pre 'fst) . >>>/pre . (boolean/pre p)))))

(: boolean/idx (Flonum -> Idx-Arrow))
(define (boolean/idx p)
  (cond [(and (p . > . 0.0) (p . < . 1.0))
         (define-values (It If) (boolean-preimage p))
         (define split (make-constant-splitter (list It If)))
         (λ (j) (list (random-index j split)))]
        [else  any/idx]))

;; ===================================================================================================
;; Total if-then-else

(: ifte*/bot* (Bot*-Arrow Bot*-Arrow Bot*-Arrow -> Bot*-Arrow))
(define (ifte*/bot* k1 k2 k3)
  (bot*-arrow
   (ifte*/bot (run/bot* branch/bot*)
              (>>>/bot (first/bot store-left/bot) (run/bot* k1))
              (>>>/bot (first/bot (>>>/bot store-right/bot store-left/bot)) (run/bot* k2))
              (>>>/bot (first/bot (>>>/bot store-right/bot store-right/bot)) (run/bot* k3)))))

(: ifte*/pre* (Pre*-Arrow Pre*-Arrow Pre*-Arrow -> Pre*-Arrow))
(define (ifte*/pre* k1 k2 k3)
  (pre*-arrow
   (ifte*/pre (run/pre* branch/pre*)
              (>>>/pre (first/pre store-left/pre) (run/pre* k1))
              (>>>/pre (first/pre (>>>/pre store-right/pre store-left/pre)) (run/pre* k2))
              (>>>/pre (first/pre (>>>/pre store-right/pre store-right/pre)) (run/pre* k3)))))

(: ifte*/idx (Idx-Arrow Idx-Arrow Idx-Arrow -> Idx-Arrow))
(define ((ifte*/idx k1 k2 k3) j)
  (append (k1 (left j))
          (list (ifte*-index j
                             (delay (k2 (left (right j))))
                             (delay (k3 (right (right j))))))))
