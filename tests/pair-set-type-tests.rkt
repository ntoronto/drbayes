#lang typed/racket

(require "../private/set/types.rkt"
         "../private/set/pair-set.rkt"
         "../private/untyped-utils.rkt"
         "../private/utils.rkt")

;; This test ensures the rect macros are sound when given abstract types and operations, by
;; wrapping them with polymorphic functions.

;; It passes if it typechecks.

(struct: Base-Rect Base-Bot-Basic () #:transparent)

(define-singleton-type Empty-Rect Base-Rect empty-rect)
(define-singleton-type Full-Rect Base-Rect pairs)

(struct: (N1 F1 N2 F2) Nonextremal-Rect Base-Rect ([fst : (U N1 F1)] [snd : (U N2 F2)])
  #:transparent)

(define-type (Nonfull-Rect N1 F1 N2 F2) (U (Nonextremal-Rect N1 F1 N2 F2) Empty-Rect))
(define-type (Nonempty-Rect N1 F1 N2 F2) (U (Nonextremal-Rect N1 F1 N2 F2) Full-Rect))
(define-type (Rect N1 F1 N2 F2) (U (Nonextremal-Rect N1 F1 N2 F2) Full-Rect Empty-Rect))

(struct: (N F E V) set-sig
  ([member? : ((U N F E) V -> Boolean)]
   [full? : (Any -> Boolean : F)]
   [empty? : (Any -> Boolean : E)]
   [full : F]
   [empty : E]
   [intersect
    : (case-> ((U N F E) (U N E) -> (U N E))
              ((U N E) (U N F E) -> (U N E))
              ((U N F E) (U N F E) -> (U N F E)))]
   [join
    : (case-> ((U N F E) (U N F) -> (U N F))
              ((U N F) (U N F E) -> (U N F))
              ((U N F E) (U N F E) -> (U N F E)))]
   [subseteq?
    : ((U N F E) (U N F E) -> Boolean)])
  #:transparent)

;; ===================================================================================================

(define-syntax-rule (define-set-sig name N F E V ops)
  (begin
    (define member? ((inst set-sig-member? N F E V) ops))
    (define full? ((inst set-sig-full? N F E V) ops))
    (define empty? ((inst set-sig-empty? N F E V) ops))
    (define full ((inst set-sig-full N F E V) ops))
    (define empty ((inst set-sig-empty N F E V) ops))
    (define intersect ((inst set-sig-intersect N F E V) ops))
    (define join ((inst set-sig-join N F E V) ops))
    (define subseteq? ((inst set-sig-subseteq? N F E V) ops))
    (define-syntax name
      (set-sig #'(N F E V)
               #'member? #'full? #'empty? #'full #'empty #'intersect #'join #'subseteq?))))

(define-syntax-rule (define-rect-sig name N1 F1 E1 V1 N2 F2 E2 V2 ops1 ops2)
  (begin
    (define ops
      ((inst set-sig (Nonextremal-Rect N1 F1 N2 F2) Full-Rect Empty-Rect (Pair V1 V2))
       ((inst construct-rect-member? N1 F1 E1 V1 N2 F2 E2 V2) ops1 ops2)
       pairs?
       empty-rect?
       pairs
       empty-rect
       ((inst construct-rect-intersect N1 F1 E1 V1 N2 F2 E2 V2) ops1 ops2)
       ((inst construct-rect-join N1 F1 E1 V1 N2 F2 E2 V2) ops1 ops2)
       ((inst construct-rect-subseteq? N1 F1 E1 V1 N2 F2 E2 V2) ops1 ops2)))
    (define-set-sig sig (Nonextremal-Rect N1 F1 N2 F2) Full-Rect Empty-Rect (Pair V1 V2) ops)
    (define-set-sig sig1 N1 F1 E1 V1 ops1)
    (define-set-sig sig2 N2 F2 E2 V2 ops2)
    (define-syntax name
      (rect-sig (syntax-local-value #'sig)
                (syntax-local-value #'sig1)
                (syntax-local-value #'sig2)
                #'(inst Nonextremal-Rect N1 F1 N2 F2)
                #'(inst Nonextremal-Rect-fst N1 F1 N2 F2)
                #'(inst Nonextremal-Rect-snd N1 F1 N2 F2)
                #'(Pair V1 V2)
                #'(inst car V1 V2)
                #'(inst cdr V1 V2)))))

;; ===================================================================================================

(define-syntax (do-rect-member? stx)
  (syntax-case stx ()
    [(_ sig)  (make-rect-member? (syntax-local-value #'sig))]))

(define-syntax (do-rect-intersect stx)
  (syntax-case stx ()
    [(_ sig)
     (make-rect-intersect (syntax-local-value #'sig))]))

(define-syntax (do-rect-join stx)
  (syntax-case stx ()
    [(_ sig)  (make-rect-join (syntax-local-value #'sig))]))

(define-syntax (do-rect-subseteq? stx)
  (syntax-case stx ()
    [(_ sig)  (make-rect-subseteq? (syntax-local-value #'sig))]))

;; ===================================================================================================

(: construct-rect-member?
   (All (N1 F1 E1 V1 N2 F2 E2 V2)
        ((set-sig N1 F1 E1 V1)
         (set-sig N2 F2 E2 V2)
         -> ((Rect N1 F1 N2 F2) (Pair V1 V2) -> Boolean))))
(define (construct-rect-member? ops1 ops2)
  (define-rect-sig sig N1 F1 E1 V1 N2 F2 E2 V2 ops1 ops2)
  (do-rect-member? sig))

(: construct-rect-intersect
   (All (N1 F1 E1 V1 N2 F2 E2 V2)
        ((set-sig N1 F1 E1 V1)
         (set-sig N2 F2 E2 V2)
         -> (case-> ((Rect N1 F1 N2 F2) (Nonfull-Rect N1 F1 N2 F2)
                                            -> (Nonfull-Rect N1 F1 N2 F2))
                    ((Nonfull-Rect N1 F1 N2 F2) (Rect N1 F1 N2 F2)
                                                    -> (Nonfull-Rect N1 F1 N2 F2))
                    ((Rect N1 F1 N2 F2) (Rect N1 F1 N2 F2) -> (Rect N1 F1 N2 F2))))))
(define (construct-rect-intersect ops1 ops2)
  (define-rect-sig sig N1 F1 E1 V1 N2 F2 E2 V2 ops1 ops2)
  (do-rect-intersect sig))

(: construct-rect-join
   (All (N1 F1 E1 V1 N2 F2 E2 V2)
        ((set-sig N1 F1 E1 V1)
         (set-sig N2 F2 E2 V2)
         -> (case-> ((Rect N1 F1 N2 F2) (Nonempty-Rect N1 F1 N2 F2)
                                            -> (Nonempty-Rect N1 F1 N2 F2))
                    ((Nonempty-Rect N1 F1 N2 F2) (Rect N1 F1 N2 F2)
                                                     -> (Nonempty-Rect N1 F1 N2 F2))
                    ((Rect N1 F1 N2 F2) (Rect N1 F1 N2 F2) -> (Rect N1 F1 N2 F2))))))
(define (construct-rect-join ops1 ops2)
  (define-rect-sig sig N1 F1 E1 V1 N2 F2 E2 V2 ops1 ops2)
  (do-rect-join sig))

(: construct-rect-subseteq?
   (All (N1 F1 E1 V1 N2 F2 E2 V2)
        ((set-sig N1 F1 E1 V1)
         (set-sig N2 F2 E2 V2)
         -> ((Rect N1 F1 N2 F2) (Rect N1 F1 N2 F2) -> Boolean))))
(define (construct-rect-subseteq? ops1 ops2)
  (define-rect-sig sig N1 F1 E1 V1 N2 F2 E2 V2 ops1 ops2)
  (do-rect-subseteq? sig))
