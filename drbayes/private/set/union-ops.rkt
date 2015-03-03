#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         "parameters.rkt"
         "types.rkt"
         "real-set.rkt"
         "prob-set.rkt"
         "null-set.rkt"
         "bool-set.rkt"
         "store.rkt"
         "store-set.rkt"
         "bottom.rkt"
         "extremal-set.rkt"
         "union.rkt"
         "value.rkt"
         "../flonum.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Pair sets
#|
(: plain-pair-set-hash (HashTable Nonempty-Set
                                  (HashTable Nonempty-Set
                                             (Weak-Boxof Plain-Pair-Set))))
(define plain-pair-set-hash (make-weak-hasheq))
|#
(: plain-pair-set (-> Nonempty-Set Nonempty-Set Plain-Pair-Set))
(define (plain-pair-set A1 A2)
  (if set-ensure-unique?
      (error 'plain-pair-set "set uniqueness unimplemented")
      #|
      (let* ([h  plain-pair-set-hash]
             [h  (hash-ref! h A1 (inst make-hasheq Nonempty-Set (Weak-Boxof Plain-Pair-Set)))])
        (weak-value-hash-ref! h A2 (λ () (Plain-Pair-Set A1 A2))))
|#
      (Plain-Pair-Set A1 A2)))

(: pair-set (case-> (-> Nonempty-Set Plain-Set Plain-Pair-Set)
                    (-> Plain-Set Nonempty-Set Plain-Pair-Set)
                    (-> Nonempty-Set Nonempty-Set Nonempty-Pair-Set)
                    (-> Nonfull-Set Set Nonfull-Pair-Set)
                    (-> Set Nonfull-Set Nonfull-Pair-Set)
                    (-> Set Set Pair-Set)))
(define (pair-set A1 A2)
  (cond [(and (universe? A1) (universe? A2))  pairs]
        [(empty-set? A1)  empty-pair-set]
        [(empty-set? A2)  empty-pair-set]
        [else  (plain-pair-set A1 A2)]))

;; ---------------------------------------------------------------------------------------------------
;; Projections

(: pair-set-fst (case-> (-> Empty-Pair-Set Empty-Set)
                        (-> Nonempty-Pair-Set Nonempty-Set)
                        (-> Pair-Set Set)))
(define (pair-set-fst A)
  (cond [(empty-pair-set? A)  empty-set]
        [(pairs? A)  universe]
        [else  (Plain-Pair-Set-fst A)]))

(: pair-set-snd (case-> (-> Empty-Pair-Set Empty-Set)
                        (-> Nonempty-Pair-Set Nonempty-Set)
                        (-> Pair-Set Set)))
(define (pair-set-snd A)
  (cond [(empty-pair-set? A)  empty-set]
        [(pairs? A)  universe]
        [else  (Plain-Pair-Set-snd A)]))

(: pair-set-projs (case-> (-> Empty-Pair-Set (Values Empty-Set Empty-Set))
                          (-> Nonempty-Pair-Set (Values Nonempty-Set Nonempty-Set))
                          (-> Pair-Set (Values Set Set))))
(define (pair-set-projs A)
  (cond [(empty-pair-set? A)  (values empty-set empty-set)]
        [(pairs? A)  (values universe universe)]
        [else  (values (Plain-Pair-Set-fst A)
                       (Plain-Pair-Set-snd A))]))

;; ---------------------------------------------------------------------------------------------------
;; Unprojections

(: pair-set-unfst (-> Pair-Set Set Pair-Set))
(define (pair-set-unfst A A1)
  (define-values (A1* A2) (pair-set-projs A))
  (let ([A1  (set-intersect A1* A1)])
    (if (eq? A1* A1) A (pair-set A1 A2))))

(: pair-set-unsnd (-> Pair-Set Set Pair-Set))
(define (pair-set-unsnd A A2)
  (define-values (A1 A2*) (pair-set-projs A))
  (let ([A2  (set-intersect A2* A2)])
    (if (eq? A2* A2) A (pair-set A1 A2))))

;; ---------------------------------------------------------------------------------------------------
;; Set operations

(: pair-set-member? (-> Pair-Set (Pair Value Value) Boolean))
(define (pair-set-member? A x)
  (cond [(empty-pair-set? A)  #f]
        [(pairs? A)  #t]
        [else  (and (set-member? (Plain-Pair-Set-fst A) (car x))
                    (set-member? (Plain-Pair-Set-snd A) (cdr x)))]))

(: pair-set-intersect (case-> (-> Pair-Set Nonfull-Pair-Set Nonfull-Pair-Set)
                              (-> Nonfull-Pair-Set Pair-Set Nonfull-Pair-Set)
                              (-> Pair-Set Pair-Set Pair-Set)))
(define (pair-set-intersect A B)
  (cond [(pairs? A)  B]
        [(pairs? B)  A]
        [(eq? A B)  A]
        [(empty-pair-set? A)  A]
        [(empty-pair-set? B)  B]
        [else
         (let* ([A1  (Plain-Pair-Set-fst A)]
                [B1  (Plain-Pair-Set-fst B)]
                [C1  (set-intersect A1 B1)])
           (if (empty-set? C1)
               empty-pair-set
               (let* ([A2  (Plain-Pair-Set-snd A)]
                      [B2  (Plain-Pair-Set-snd B)]
                      [C2  (set-intersect A2 B2)])
                 (if (empty-set? C2)
                     empty-pair-set
                     (cond [(and (eq? A1 C1) (eq? A2 C2))  A]
                           [(and (eq? B1 C1) (eq? B2 C2))  B]
                           [else  (plain-pair-set C1 C2)])))))]))

(: pair-set-join (case-> (-> Pair-Set Nonempty-Pair-Set (Values Nonempty-Pair-Set Boolean))
                         (-> Nonempty-Pair-Set Pair-Set (Values Nonempty-Pair-Set Boolean))
                         (-> Pair-Set Pair-Set (Values Pair-Set Boolean))))
(define (pair-set-join A B)
  (cond [(empty-pair-set? A)  (values B #t)]
        [(empty-pair-set? B)  (values A #t)]
        [(eq? A B)  (values A #t)]
        [(pairs? A)  (values A #t)]
        [(pairs? B)  (values B #t)]
        [else
         (define A1 (Plain-Pair-Set-fst A))
         (define A2 (Plain-Pair-Set-snd A))
         (define B1 (Plain-Pair-Set-fst B))
         (define B2 (Plain-Pair-Set-snd B))
         (define-values (C1 exact1?) (set-join A1 B1))
         (define-values (C2 exact2?) (set-join A2 B2))
         (cond [(and (universe? C1) (universe? C2))  (values pairs (and exact1? exact2?))]
               [(and (eq? A1 C1) (eq? A2 C2))  (values A #t)]
               [(and (eq? B1 C1) (eq? B2 C2))  (values B #t)]
               [else
                (define exact?
                  (or (and exact2? (eq? A1 B1))
                      (and exact1? (eq? A2 B2))))
                (values (pair-set C1 C2) exact?)])]))

(: pair-set-subseteq? (-> Pair-Set Pair-Set Boolean))
(define (pair-set-subseteq? A B)
  (cond [(eq? A B)  #t]
        [(or (empty-pair-set? A) (pairs? B))  #t]
        [(or (pairs? A) (empty-pair-set? B))  #f]
        [else  (and (set-subseteq? (Plain-Pair-Set-fst A) (Plain-Pair-Set-fst B))
                    (set-subseteq? (Plain-Pair-Set-snd A) (Plain-Pair-Set-snd B)))]))

(: pair-set-singleton? (-> Pair-Set Boolean))
(define (pair-set-singleton? A)
  (cond [(or (empty-pair-set? A) (pairs? A))  #f]
        [else  (and (set-singleton? (Plain-Pair-Set-fst A))
                    (set-singleton? (Plain-Pair-Set-snd A)))]))

;; ===================================================================================================
;; Basic set ops

(define-singleton-type Different different)

(: basic-member? (-> Basic Value (U Different Boolean)))
(define (basic-member? A x)
  (cond [(and (real-set? A) (flonum? x))   (real-set-member? A x)]
        [(and (prob-set? A) (prob? x))     (prob-set-member? A x)]
        [(and (bool-set? A) (boolean? x))  (bool-set-member? A x)]
        [(and (null-set? A) (null? x))     (null-set-member? A x)]
        [(and (pair-set? A) (pair? x))     (pair-set-member? A x)]
        [(and (store-set? A) (store? x))   (store-set-member? A x)]
        [else  different]))

(: basic-intersect (case-> (-> Nonfull-Basic Basic (U Different Nonfull-Basic))
                           (-> Basic Nonfull-Basic (U Different Nonfull-Basic))
                           (-> Basic Basic (U Different Basic))))
(define (basic-intersect A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-intersect A B)]
        [(and (prob-set? A) (prob-set? B))  (prob-set-intersect A B)]
        [(and (null-set? A) (null-set? B))  (null-set-intersect A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-intersect A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-intersect A B)]
        [(and (store-set? A) (store-set? B))  (store-set-intersect A B)]
        [else  different]))

(: basic-join (case-> (-> Nonempty-Basic Basic (Values (U Different Nonempty-Basic) Boolean))
                      (-> Basic Nonempty-Basic (Values (U Different Nonempty-Basic) Boolean))
                      (-> Basic Basic (Values (U Different Basic) Boolean))))
(define (basic-join A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-join A B)]
        [(and (prob-set? A) (prob-set? B))  (prob-set-join A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-join A B)]
        [(and (null-set? A) (null-set? B))  (null-set-join A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-join A B)]
        [(and (store-set? A) (store-set? B))  (store-set-join A B)]
        [else  (values different #t)]))

(: basic-subseteq? (-> Basic Basic (U Different Boolean)))
(define (basic-subseteq? A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-subseteq? A B)]
        [(and (prob-set? A) (prob-set? B))  (prob-set-subseteq? A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-subseteq? A B)]
        [(and (null-set? A) (null-set? B))  (null-set-subseteq? A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-subseteq? A B)]
        [(and (store-set? A) (store-set? B))  (store-set-subseteq? A B)]
        [else  different]))

(: basic-singleton? (-> Basic Boolean))
(define (basic-singleton? A)
  (cond [(real-set? A)  (real-set-singleton? A)]
        [(prob-set? A)  (prob-set-singleton? A)]
        [(bool-set? A)  (bool-set-singleton? A)]
        [(null-set? A)  (null-set-singleton? A)]
        [(pair-set? A)  (pair-set-singleton? A)]
        ;; Store sets can't be singletons
        [else  #f]))

;; ===================================================================================================
;; Membership

(: set-member? (-> Set Maybe-Value Boolean))
(define (set-member? A x)
  (cond [(bottom? x)       #f]
        [(empty-set? A)    #f]
        [(not (value? x))  #f]
        [(universe? A)     #t]
        [(bot-set? A)
         (cond [(bot-basic? A)  (bot-basic-member? A x)]
               [(bot-tagged? A)  (bot-tagged-member? A x)]
               [else  (bot-union-member? A x)])]
        [else
         (cond [(top-basic? A)  (top-basic-member? A x)]
               [(top-tagged? A)  (top-tagged-member? A x)]
               [else  (top-union-member? A x)])]))

(: bot-basic-member? (-> Bot-Basic Value Boolean))
(define (bot-basic-member? A x)
  (define res (basic-member? A x))
  (if (different? res) #f res))

(: top-basic-member? (-> Top-Basic Value Boolean))
(define (top-basic-member? A x)
  (define Asub (top-basic-set A))
  (define res (basic-member? Asub x))
  (if (different? res) #t res))

(: bot-tagged-member? (-> Bot-Tagged Value Boolean))
(define (bot-tagged-member? A x)
  (and (tagged-value? x)
       (eq? (bot-tagged-tag A) (tagged-value-tag x))
       (set-member? (bot-tagged-set A) (tagged-value-value x))))

(: top-tagged-member? (-> Top-Tagged Value Boolean))
(define (top-tagged-member? A x)
  (or (not (tagged-value? x))
      (not (eq? (top-tagged-tag A) (tagged-value-tag x)))
      (set-member? (top-tagged-set A) (tagged-value-value x))))

(: bot-union-member? (-> Bot-Union Value Boolean))
(define (bot-union-member? A x)
  (set-member? (bot-union-ref A (value-tag x)) x))

(: top-union-member? (-> Top-Union Value Boolean))
(define (top-union-member? A x)
  (set-member? (top-union-ref A (value-tag x)) x))

;; ===================================================================================================
;; Join

(: set-join (case-> (-> Set Nonempty-Set (Values Nonempty-Set Boolean))
                    (-> Nonempty-Set Set (Values Nonempty-Set Boolean))
                    (-> Set Set (Values Set Boolean))))
(define (set-join A B)
  (cond
    [(empty-set? A)  (values B #t)]
    [(empty-set? B)  (values A #t)]
    [(eq? A B)  (values B #t)]
    [(universe? A)   (values A #t)]
    [(universe? B)   (values B #t)]
    [(bot-set? A)
     (if (bot-set? B)
         (if (and (bot-entry? A) (bot-entry? B)) (bot-bot-entry-join A B) (bot-bot-join A B))
         (if (and (bot-entry? A) (top-entry? B)) (top-bot-entry-join B A) (top-bot-join B A)))]
    [else
     (if (bot-set? B)
         (if (and (top-entry? A) (bot-entry? B)) (top-bot-entry-join A B) (top-bot-join A B))
         (if (and (top-entry? A) (top-entry? B)) (top-top-entry-join B A) (top-top-join B A)))]))

(: bot-bot-join (-> (U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union) (Values Nonempty-Set Boolean)))
(define (bot-bot-join A B)
  (for/fold ([A A] [exact? : Boolean  #t])
            ([Bsub  (in-list (bot-union-sets B))])
    (define b-tag (bot-tag Bsub))
    (define Asub (bot-union-ref A b-tag))
    (define-values (Dsub e?) (if (empty-set? Asub)
                                 (values Bsub #t)
                                 (bot-bot-entry-join Asub Bsub)))
    (values (bot-union-add A Dsub) (and exact? e?))))

(: top-bot-join (-> (U Top-Entry Top-Union) (U Bot-Entry Bot-Union) (Values Nonempty-Set Boolean)))
(define (top-bot-join A B)
  (for/fold ([A : (U Universe Top-Entry Top-Union)  A] [exact? : Boolean  #t])
            ([Asub  (in-list (top-union-sets A))])
    (define a-tag (top-tag Asub))
    (define Bsub (bot-union-ref B a-tag))
    (define-values (Dsub e?) (if (empty-set? Bsub)
                                 (values Asub #t)
                                 (top-bot-entry-join Asub Bsub)))
    (values (if (universe? Dsub) (top-union-remove A a-tag) (top-union-add A Dsub))
            (and exact? e?))))

(: top-top-join (-> (U Top-Entry Top-Union) (U Top-Entry Top-Union) (Values Nonempty-Set Boolean)))
(define (top-top-join A B)
  (for/fold: ([C : (U Universe Top-Entry Top-Union)  universe] [exact? : Boolean  #t])
             ([Bsub  (in-list (top-union-sets B))])
    (define b-tag (top-tag Bsub))
    (define Asub (top-union-ref A b-tag))
    (define-values (Dsub e?) (if (universe? Asub)
                                 (values universe #t)
                                 (top-top-entry-join Asub Bsub)))
    (values (if (universe? Dsub) C (top-union-add C Dsub))
            (and exact? e?))))

(: bot-bot-entry-join (-> Bot-Entry Bot-Entry (Values (U Bot-Entry Bot-Union) Boolean)))
(define (bot-bot-entry-join A B)
  (cond [(and (bot-basic? A)  (bot-basic? B))   (bot-bot-basic-join A B)]
        [(and (bot-tagged? A) (bot-tagged? B))  (bot-bot-tagged-join A B)]
        [else  (values (bot-union A B) #t)]))

(: top-bot-entry-join (-> Top-Entry Bot-Entry (Values (U Top-Entry Top-Union Universe) Boolean)))
(define (top-bot-entry-join A B)
  (cond [(and (top-basic? A)  (bot-basic? B))   (top-bot-basic-join A B)]
        [(and (top-tagged? A) (bot-tagged? B))  (top-bot-tagged-join A B)]
        [else  (values A #t)]))

(: top-top-entry-join (-> Top-Entry Top-Entry (Values (U Top-Entry Top-Union Universe) Boolean)))
(define (top-top-entry-join A B)
  (cond [(and (top-basic? A)  (top-basic? B))   (top-top-basic-join A B)]
        [(and (top-tagged? A) (top-tagged? B))  (top-top-tagged-join A B)]
        [else  (values universe #t)]))

(: bot-bot-basic-join (-> Bot-Basic Bot-Basic (Values (U Bot-Basic Bot-Union) Boolean)))
(define (bot-bot-basic-join A B)
  (define-values (C exact?) (basic-join A B))
  (cond [(different? C)  (values (bot-union A B) #t)]
        [(eq? C A)  (values A #t)]
        [(eq? C B)  (values B #t)]
        [else  (values C exact?)]))

(: top-bot-basic-join (-> Top-Basic Bot-Basic (Values (U Top-Basic Top-Union Universe) Boolean)))
(define (top-bot-basic-join A B)
  (define Asub (top-basic-set A))
  (define-values (Csub exact?) (basic-join Asub B))
  (cond [(different? Csub)  (values A #t)]
        [(eq? Csub Asub)  (values A #t)]
        [else  (values (top-basic Csub) exact?)]))

(: top-top-basic-join (-> Top-Basic Top-Basic (Values (U Top-Basic Top-Union Universe) Boolean)))
(define (top-top-basic-join A B)
  (define Asub (top-basic-set A))
  (define Bsub (top-basic-set B))
  (define-values (Csub exact?) (basic-join Asub Bsub))
  (cond [(different? Csub)  (values universe #t)]
        [(eq? Csub Asub)  (values A #t)]
        [(eq? Csub Bsub)  (values B #t)]
        [else  (values (top-basic Csub) exact?)]))

(: bot-bot-tagged-join (-> Bot-Tagged Bot-Tagged (Values (U Bot-Tagged Bot-Union) Boolean)))
(define (bot-bot-tagged-join A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define-values (Csub exact?) (set-join Asub Bsub))
         (cond [(eq? Csub Asub)  (values A #t)]
               [(eq? Csub Bsub)  (values B #t)]
               [else  (values (bot-tagged a-tag Csub) exact?)])]
        [else  (values (bot-union A B) #t)]))

(: top-bot-tagged-join (-> Top-Tagged Bot-Tagged (Values (U Top-Tagged Top-Union Universe) Boolean)))
(define (top-bot-tagged-join A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define-values (Csub exact?) (set-join Asub Bsub))
         (cond [(eq? Csub Asub)  (values A #t)]
               [else  (values (top-tagged a-tag Csub) exact?)])]
        [else  (values A #t)]))

(: top-top-tagged-join (-> Top-Tagged Top-Tagged (Values (U Top-Tagged Top-Union Universe) Boolean)))
(define (top-top-tagged-join A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define-values (Csub exact?) (set-join Asub Bsub))
         (cond [(eq? Csub Asub)  (values A #t)]
               [(eq? Csub Bsub)  (values B #t)]
               [else  (values (top-tagged a-tag Csub) exact?)])]
        [else  (values universe #t)]))

;; ===================================================================================================
;; Intersection

(: set-intersect (case-> (-> Set Nonfull-Set Nonfull-Set)
                         (-> Nonfull-Set Set Nonfull-Set)
                         (-> Set Set Set)))
(define (set-intersect A B)
  (cond [(universe? A)   B]
        [(universe? B)   A]
        [(eq? A B)  B]
        [(empty-set? A)  A]
        [(empty-set? B)  B]
        [(bot-set? A)
         (if (bot-set? B)
             (cond [(and (bot-entry? A) (bot-entry? B))  (bot-bot-entry-intersect A B)]
                   [else  (bot-bot-intersect A B)])
             (cond [(and (bot-entry? A) (top-entry? B))  (bot-top-entry-intersect A B)]
                   [else  (bot-top-intersect A B)]))]
        [else
         (if (bot-set? B)
             (cond [(and (top-entry? A) (bot-entry? B))  (bot-top-entry-intersect B A)]
                   [else  (bot-top-intersect B A)])
             (cond [(and (top-entry? A) (top-entry? B))  (top-top-entry-intersect A B)]
                   [else  (top-top-intersect A B)]))]))

(: bot-bot-intersect (-> (U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union)
                         (U Empty-Set Bot-Entry Bot-Union)))
(define (bot-bot-intersect A B)
  (for/fold: ([C : (U Empty-Set Bot-Entry Bot-Union)  empty-set]
              ) ([Bsub  (in-list (bot-union-sets B))])
    (define b-tag (bot-tag Bsub))
    (define Asub (bot-union-ref A b-tag))
    (define Dsub (if (empty-set? Asub) empty-set (bot-bot-entry-intersect Asub Bsub)))
    (if (empty-set? Dsub) C (bot-union-add C Dsub))))

(: bot-top-intersect (-> (U Bot-Entry Bot-Union) (U Top-Entry Top-Union)
                         (U Bot-Entry Bot-Union Empty-Set)))
(define (bot-top-intersect A B)
  (for/fold: ([A : (U Empty-Set Bot-Entry Bot-Union)  A]) ([Asub  (in-list (bot-union-sets A))])
    (define a-tag (bot-tag Asub))
    (define Bsub (top-union-ref B a-tag))
    (define Dsub (if (universe? Bsub) Asub (bot-top-entry-intersect Asub Bsub)))
    (if (empty-set? Dsub) (bot-union-remove A a-tag) (bot-union-add A Dsub))))

(: top-top-intersect (-> (U Top-Entry Top-Union) (U Top-Entry Top-Union) (U Top-Entry Top-Union)))
(define (top-top-intersect A B)
  (for/fold ([A A]) ([Bsub  (in-list (top-union-sets B))])
    (define b-tag (top-tag Bsub))
    (define Asub (top-union-ref A b-tag))
    (define Dsub (if (universe? Asub) Bsub (top-top-entry-intersect Asub Bsub)))
    (top-union-add A Dsub)))

(: bot-bot-entry-intersect (-> Bot-Entry Bot-Entry (U Bot-Entry Empty-Set)))
(define (bot-bot-entry-intersect A B)
  (cond [(and (bot-basic? A)  (bot-basic? B))   (bot-bot-basic-intersect A B)]
        [(and (bot-tagged? A) (bot-tagged? B))  (bot-bot-tagged-intersect A B)]
        [else  empty-set]))

(: bot-top-entry-intersect (-> Bot-Entry Top-Entry (U Bot-Entry Empty-Set)))
(define (bot-top-entry-intersect A B)
  (cond [(and (bot-basic? A)  (top-basic? B))   (bot-top-basic-intersect A B)]
        [(and (bot-tagged? A) (top-tagged? B))  (bot-top-tagged-intersect A B)]
        [else  A]))

(: top-top-entry-intersect (-> Top-Entry Top-Entry (U Top-Entry Top-Union)))
(define (top-top-entry-intersect A B)
  (cond [(and (top-basic? A)  (top-basic? B))   (top-top-basic-intersect A B)]
        [(and (top-tagged? A) (top-tagged? B))  (top-top-tagged-intersect A B)]
        [else  (top-union A B)]))

(: bot-bot-basic-intersect (-> Bot-Basic Bot-Basic (U Bot-Basic Empty-Set)))
(define (bot-bot-basic-intersect A B)
  (define C (basic-intersect A B))
  (cond [(different? C)  empty-set]
        [else  (bot-basic C)]))

(: bot-top-basic-intersect (-> Bot-Basic Top-Basic (U Bot-Basic Empty-Set)))
(define (bot-top-basic-intersect A B)
  (define Bsub (top-basic-set B))
  (define Csub (basic-intersect A Bsub))
  (cond [(different? Csub)  A]
        [else  (bot-basic Csub)]))

(: top-top-basic-intersect (-> Top-Basic Top-Basic (U Top-Basic Top-Union)))
(define (top-top-basic-intersect A B)
  (define Asub (top-basic-set A))
  (define Bsub (top-basic-set B))
  (define Csub (basic-intersect Asub Bsub))
  (cond [(different? Csub)  (top-union A B)]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (top-basic Csub)]))

(: bot-bot-tagged-intersect (-> Bot-Tagged Bot-Tagged (U Bot-Tagged Empty-Set)))
(define (bot-bot-tagged-intersect A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (bot-tagged a-tag Csub)])]
        [else  empty-set]))

(: bot-top-tagged-intersect (-> Bot-Tagged Top-Tagged (U Bot-Tagged Empty-Set)))
(define (bot-top-tagged-intersect A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [else  (bot-tagged a-tag Csub)])]
        [else  A]))

(: top-top-tagged-intersect (-> Top-Tagged Top-Tagged (U Top-Tagged Top-Union)))
(define (top-top-tagged-intersect A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (top-tagged a-tag Csub)])]
        [else  (top-union A B)]))

;; ===================================================================================================
;; Subset or equal

(: set-subseteq? (-> Set Set Boolean))
(define (set-subseteq? A B)
  (cond [(eq? A B)  #t]
        [(empty-set? A)  #t]
        [(empty-set? B)  #f]
        [(universe? B)   #t]
        [(universe? A)   #f]
        [(bot-set? A)
         (if (bot-set? B)
             (cond [(and (bot-entry? A) (bot-entry? B))  (bot-bot-entry-subseteq? A B)]
                   [else  (bot-bot-subseteq? A B)])
             (cond [(and (bot-entry? A) (top-entry? B))  (bot-top-entry-subseteq? A B)]
                   [else  (bot-top-subseteq? A B)]))]
        [else
         (if (bot-set? B)
             #f
             (cond [(and (top-entry? A) (top-entry? B))  (top-top-entry-subseteq? A B)]
                   [else  (top-top-subseteq? A B)]))]))

(: bot-bot-subseteq? (-> (U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union) Boolean))
(define (bot-bot-subseteq? A B)
  (andmap (λ: ([Asub : Bot-Entry])
            (define Bsub (bot-union-ref B (bot-tag Asub)))
            (if (empty-set? Bsub) #f (bot-bot-entry-subseteq? Asub Bsub)))
          (bot-union-sets A)))

(: bot-top-subseteq? (-> (U Bot-Entry Bot-Union) (U Top-Entry Top-Union) Boolean))
(define (bot-top-subseteq? A B)
  (andmap (λ: ([Asub : Bot-Entry])
            (define Bsub (top-union-ref B (bot-tag Asub)))
            (if (universe? Bsub) #t (bot-top-entry-subseteq? Asub Bsub)))
          (bot-union-sets A)))

(: top-top-subseteq? (-> (U Top-Entry Top-Union) (U Top-Entry Top-Union) Boolean))
(define (top-top-subseteq? A B)
  (andmap (λ: ([Bsub : Top-Entry])
            (define Asub (top-union-ref A (top-tag Bsub)))
            (if (universe? Asub) #f (top-top-entry-subseteq? Asub Bsub)))
          (top-union-sets B)))

(: bot-bot-entry-subseteq? (-> Bot-Entry Bot-Entry Boolean))
(define (bot-bot-entry-subseteq? A B)
  (cond [(and (bot-basic? A)  (bot-basic? B))   (bot-bot-basic-subseteq? A B)]
        [(and (bot-tagged? A) (bot-tagged? B))  (bot-bot-tagged-subseteq? A B)]
        [else  #f]))

(: bot-top-entry-subseteq? (-> Bot-Entry Top-Entry Boolean))
(define (bot-top-entry-subseteq? A B)
  (cond [(and (bot-basic? A)  (top-basic? B))   (bot-top-basic-subseteq? A B)]
        [(and (bot-tagged? A) (top-tagged? B))  (bot-top-tagged-subseteq? A B)]
        [else  #t]))

(: top-top-entry-subseteq? (-> Top-Entry Top-Entry Boolean))
(define (top-top-entry-subseteq? A B)
  (cond [(and (top-basic? A)  (top-basic? B))   (top-top-basic-subseteq? A B)]
        [(and (top-tagged? A) (top-tagged? B))  (top-top-tagged-subseteq? A B)]
        [else  #f]))

(: bot-bot-basic-subseteq? (-> Bot-Basic Bot-Basic Boolean))
(define (bot-bot-basic-subseteq? A B)
  (define res (basic-subseteq? A B))
  (if (different? res) #f res))

(: bot-top-basic-subseteq? (-> Bot-Basic Top-Basic Boolean))
(define (bot-top-basic-subseteq? A B)
  (define res (basic-subseteq? A (top-basic-set B)))
  (if (different? res) #t res))

(: top-top-basic-subseteq? (-> Top-Basic Top-Basic Boolean))
(define (top-top-basic-subseteq? A B)
  (define res (basic-subseteq? (top-basic-set A) (top-basic-set B)))
  (if (different? res) #f res))

(: bot-bot-tagged-subseteq? (-> Bot-Tagged Bot-Tagged Boolean))
(define (bot-bot-tagged-subseteq? A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (set-subseteq? (bot-tagged-set A) (bot-tagged-set B))]
        [else  #f]))

(: bot-top-tagged-subseteq? (-> Bot-Tagged Top-Tagged Boolean))
(define (bot-top-tagged-subseteq? A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (set-subseteq? (bot-tagged-set A) (top-tagged-set B))]
        [else  #t]))

(: top-top-tagged-subseteq? (-> Top-Tagged Top-Tagged Boolean))
(define (top-top-tagged-subseteq? A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (set-subseteq? (top-tagged-set A) (top-tagged-set B))]
        [else  #f]))

;; ===================================================================================================
;; Singleton test

(: set-singleton? (-> Set Boolean))
(define (set-singleton? A)
  (cond [(empty-set? A)  #f]
        [(universe? A)   #f]
        [(bot-set? A)
         (cond [(bot-basic? A)   (bot-basic-singleton? A)]
               [(bot-tagged? A)  (bot-tagged-singleton? A)]
               [else  #f])]
        ;; Top sets can't be singletons
        [else  #f]))

(: bot-basic-singleton? (-> Bot-Basic Boolean))
(define (bot-basic-singleton? A)
  (basic-singleton? A))

(: bot-tagged-singleton? (-> Bot-Tagged Boolean))
(define (bot-tagged-singleton? A)
  (set-singleton? (bot-tagged-set A)))

;; ===================================================================================================
;; Singleton

(: value->singleton (Value -> Bot-Entry))
(define (value->singleton v)
  (cond [(flonum? v)   (flonum->singleton v)]
        [(boolean? v)  (boolean->singleton v)]
        [(null? v)     nulls]
        [(pair? v)     (pair->singleton v)]
        [(tagged-value? v)  (tagged-value->singleton v)]
        [else  (raise-argument-error 'value->singleton
                                     "Flonum, Boolean, Null, Pair or tagged-value" v)]))

(: pair->singleton ((Pair Value Value) -> Bot-Basic))
(define (pair->singleton x)
  (plain-pair-set (value->singleton (car x))
                  (value->singleton (cdr x))))

(: tagged-value->singleton (tagged-value -> Bot-Tagged))
(define (tagged-value->singleton v)
  (bot-tagged (tagged-value-tag v)
              (value->singleton (tagged-value-value v))))
