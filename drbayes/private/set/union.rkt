#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         racket/match
         math/flonum
         math/private/utils
         "types.rkt"
         "real-set.rkt"
         "prob-set.rkt"
         "bool-set.rkt"
         "null-set.rkt"
         "extremal-set.rkt"
         "store-set.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Tags

;; Basic set tags
(define real-tag 'real)
(define prob-tag 'prob)
(define bool-tag 'bool)
(define null-tag 'null)
(define pair-tag 'pair)
(define store-tag 'store)

;; Set tags
(: make-set-tag (-> Symbol Tag))
(define (make-set-tag sym)
  (string->uninterned-symbol (symbol->string sym)))

;; ===================================================================================================
;; Set types

(define-type Plain-Set
  (U Bot-Basic Top-Basic
     Bot-Tagged Top-Tagged
     Bot-Union Top-Union))

(define-type Nonempty-Set (U Plain-Set Universe))
(define-type  Nonfull-Set (U Plain-Set Empty-Set))
(define-type          Set (U Plain-Set Universe Empty-Set))

;; ===================================================================================================
;; Pairs

(struct: Base-Pair-Set Base-Bot-Basic () #:transparent)
(define pair-set? Base-Pair-Set?)

(define-singleton-type Empty-Pair-Set Base-Pair-Set empty-pair-set)
(define-singleton-type Full-Pair-Set Base-Pair-Set pairs)

(struct: Plain-Pair-Set Base-Pair-Set ([fst : Nonempty-Set] [snd : Nonempty-Set]) #:transparent)

(define-type Nonfull-Pair-Set (U Plain-Pair-Set Empty-Pair-Set))
(define-type Nonempty-Pair-Set (U Plain-Pair-Set Full-Pair-Set))
(define-type Pair-Set (U Plain-Pair-Set Full-Pair-Set Empty-Pair-Set))

;; ===================================================================================================
;; Basic sets

(define-type Full-Basic
  (U Full-Real-Set
     Full-Prob-Set
     Full-Bool-Set
     Full-Null-Set
     Full-Pair-Set
     Full-Store-Set))

(define-type Empty-Basic
  (U Empty-Real-Set
     Empty-Prob-Set
     Empty-Bool-Set
     Empty-Null-Set
     Empty-Pair-Set
     Empty-Store-Set))

(define-type Plain-Basic
  (U Plain-Real-Set
     Plain-Prob-Set
     Plain-Bool-Set
     ;Plain-Null-Set  ; there aren't any plain null sets
     Plain-Pair-Set
     Plain-Store-Set))

(define-type Nonempty-Basic (U Plain-Basic Full-Basic))
(define-type  Nonfull-Basic (U Plain-Basic Empty-Basic))
(define-type          Basic (U Plain-Basic Full-Basic Empty-Basic))

(: empty-basic? (-> Any Boolean : Empty-Basic))
(define (empty-basic? A)
  (or (empty-real-set? A)
      (empty-prob-set? A)
      (empty-bool-set? A)
      (empty-null-set? A)
      (empty-pair-set? A)
      (empty-store-set? A)))

(: full-basic? (-> Any Boolean : Full-Basic))
(define (full-basic? A)
  (or (reals? A)
      (probs? A)
      (bools? A)
      (nulls? A)
      (pairs? A)
      (stores? A)))

;; ===================================================================================================
;; Top and bottom basic sets

(define-type Bot-Basic Nonempty-Basic)

#|
(define bot-basic-set Bot-Basic-set)
(define bot-basic? Bot-Basic?)
|#

(struct: Top-Basic Base-Top-Entry ([set : Nonfull-Basic]) #:transparent)

(define top-basic-set Top-Basic-set)
(define top-basic? Top-Basic?)

(: basic-tag (-> Basic Tag))
(define (basic-tag A)
  (cond [(real-set? A)  real-tag]
        [(prob-set? A)  prob-tag]
        [(bool-set? A)  bool-tag]
        [(null-set? A)  null-tag]
        [(pair-set? A)  pair-tag]
        [(store-set? A)  store-tag]))

(: top-basic-tag (-> Top-Basic Tag))
(define (top-basic-tag A) (basic-tag (top-basic-set A)))

(: bot-basic (case-> (-> Bot-Basic Bot-Basic)
                     (-> Basic (U Bot-Basic Empty-Set))))
(define (bot-basic A)
  (if (empty-basic? A) empty-set A))

(: top-basic (case-> (-> Nonfull-Basic Top-Basic)
                     (-> Basic (U Top-Basic Universe))))
(define (top-basic A)
  (if (full-basic? A) universe (Top-Basic A)))

(define not-reals (Top-Basic empty-real-set))
(define not-probs (Top-Basic empty-prob-set))
(define not-bools (Top-Basic empty-bool-set))
(define not-nulls (Top-Basic empty-null-set))
(define not-pairs (Top-Basic empty-pair-set))
(define not-store-set (Top-Basic empty-store-set))

;; ===================================================================================================
;; Tagged sets

(struct: Bot-Tagged Base-Bot-Entry ([tag : Tag] [set : Nonempty-Set]) #:transparent)
(struct: Top-Tagged Base-Top-Entry ([tag : Tag] [set : Nonfull-Set]) #:transparent)

(define bot-tagged-tag Bot-Tagged-tag)
(define bot-tagged-set Bot-Tagged-set)
(define bot-tagged? Bot-Tagged?)

(define top-tagged-tag Top-Tagged-tag)
(define top-tagged-set Top-Tagged-set)
(define top-tagged? Top-Tagged?)

(: bot-tagged (case-> (-> Tag Nonempty-Set Bot-Tagged)
                      (-> Tag Set (U Bot-Tagged Empty-Set))))
(define (bot-tagged tag A)
  (if (empty-set? A) A (Bot-Tagged tag A)))

(: top-tagged (case-> (-> Tag Nonfull-Set Top-Tagged)
                      (-> Tag Set (U Top-Tagged Universe))))
(define (top-tagged tag A)
  (if (universe? A) A (Top-Tagged tag A)))

;; ===================================================================================================
;; Unions

(define-type Bot-Entry (U Bot-Basic Bot-Tagged))
(define-type Top-Entry (U Top-Basic Top-Tagged))

(: bot-tag (-> Bot-Entry Tag))
(define (bot-tag A)
  (if (bot-basic? A) (basic-tag A) (bot-tagged-tag A)))

(: top-tag (-> Top-Entry Tag))
(define (top-tag A)
  (if (top-basic? A) (top-basic-tag A) (top-tagged-tag A)))

(define-type Bot-Union-Hash (HashTable Tag Bot-Entry))
(define-type Top-Union-Hash (HashTable Tag Top-Entry))

(struct: Bot-Union Base-Bot-Set ([hash : Bot-Union-Hash]) #:transparent)
(struct: Top-Union Base-Top-Set ([hash : Top-Union-Hash]) #:transparent)

(define bot-union? Bot-Union?)
(define top-union? Top-Union?)

(: bot-union (-> Bot-Entry Bot-Entry Bot-Union))
(define (bot-union A0 A1)
  (Bot-Union ((inst hasheq2 Tag Bot-Entry) (bot-tag A0) A0 (bot-tag A1) A1)))

(: top-union (-> Top-Entry Top-Entry Top-Union))
(define (top-union A0 A1)
  (Top-Union ((inst hasheq2 Tag Top-Entry) (top-tag A0) A0 (top-tag A1) A1)))

(: bot-union-sets (-> (U Empty-Set Bot-Entry Bot-Union) (Listof Bot-Entry)))
(define (bot-union-sets A)
  (cond [(empty-set? A)  empty]
        [(or (bot-basic? A) (bot-tagged? A))  (list A)]
        [else  (hash-values (Bot-Union-hash A))]))

(: top-union-sets (-> (U Universe Top-Entry Top-Union) (Listof Top-Entry)))
(define (top-union-sets A)
  (cond [(universe? A)  empty]
        [(or (top-basic? A) (top-tagged? A))  (list A)]
        [else  (hash-values (Top-Union-hash A))]))

(: bot-union-ref (-> (U Empty-Set Bot-Entry Bot-Union) Tag (U Empty-Set Bot-Entry)))
(define (bot-union-ref A tag)
  (cond [(empty-set? A)  A]
        [(bot-entry? A)  (if (eq? tag (bot-tag A)) A empty-set)]
        [else  (hash-ref (Bot-Union-hash A) tag (λ () empty-set))]))

(: top-union-ref (-> (U Universe Top-Entry Top-Union) Tag (U Universe Top-Entry)))
(define (top-union-ref A tag)
  (cond [(universe? A)   A]
        [(top-entry? A)  (if (eq? tag (top-tag A)) A universe)]
        [else  (hash-ref (Top-Union-hash A) tag (λ () universe))]))

(: bot-union-add (-> (U Empty-Set Bot-Entry Bot-Union) (U Bot-Entry Bot-Union)
                     (U Bot-Entry Bot-Union)))
(define (bot-union-add A C)
  (cond [(empty-set? A)  C]
        [(bot-union? C)  (for/fold ([A A]) ([C  (in-list (bot-union-sets C))])
                           (bot-union-add A C))]
        [(bot-entry? A)
         (define a-tag (bot-tag A))
         (define c-tag (bot-tag C))
         (cond [(and (bot-entry? C) (eq? c-tag a-tag))  C]
               [else  (Bot-Union (hasheq2 a-tag A c-tag C))])]
        [else
         (Bot-Union (hash-set (Bot-Union-hash A) (bot-tag C) C))]))

(: bot-union-remove (-> (U Empty-Set Bot-Entry Bot-Union) Tag (U Empty-Set Bot-Entry Bot-Union)))
(define (bot-union-remove A tag)
  (cond [(empty-set? A)  A]
        [(bot-entry? A)  (if (eq? (bot-tag A) tag) empty-set A)]
        [else  (define h (hash-remove (Bot-Union-hash A) tag))
               (define n (hash-count h))
               (cond [(= n 0)  empty-set]
                     [(= n 1)  (first (hash-values h))]
                     [else  (Bot-Union h)])]))

(: top-union-add (-> (U Universe Top-Entry Top-Union) (U Top-Entry Top-Union)
                     (U Top-Entry Top-Union)))
(define (top-union-add A C)
  (cond [(universe? A)  C]
        [(top-union? C)  (for/fold ([A A]) ([C  (in-list (top-union-sets C))])
                           (top-union-add A C))]
        [(top-entry? A)
         (define a-tag (top-tag A))
         (define c-tag (top-tag C))
         (cond [(and (top-entry? C) (eq? c-tag a-tag))  C]
               [else  (Top-Union (hasheq2 a-tag A c-tag C))])]
        [else
         (Top-Union (hash-set (Top-Union-hash A) (top-tag C) C))]))

(: top-union-remove (-> (U Universe Top-Entry Top-Union) Tag (U Universe Top-Entry Top-Union)))
(define (top-union-remove A tag)
  (cond [(universe? A)   A]
        [(top-entry? A)  (if (eq? (top-tag A) tag) universe A)]
        [else  (define h (hash-remove (Top-Union-hash A) tag))
               (define n (hash-count h))
               (cond [(= n 0)  universe]
                     [(= n 1)  (first (hash-values h))]
                     [else  (Top-Union h)])]))
