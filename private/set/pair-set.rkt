#lang racket/base

#|
TODO: Look up self-balancing quadtrees to change set ops from O(n^2) to O(n*log(n))
|#

(require (for-syntax racket/base
                     racket/syntax)
         racket/list
         typed/racket/base
         "types.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

;; ===================================================================================================
;; Signatures for abstract set data types

(begin-for-syntax
  (struct set-sig (types member? full? empty? full empty intersect join subseteq?)
    #:transparent)
  
  (struct rect-sig (set-sig
                    fst-set-sig snd-set-sig
                    rect rect-fst rect-snd
                    value value-fst value-snd)
    #:transparent)
  )

;; ---------------------------------------------------------------------------------------------------
;; Typed binding getters for sets

(define-for-syntax (set-sig-member?-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types sig)])
    #`(: ((U N F E) V -> Boolean)
         #,(set-sig-member? sig))))

(define-for-syntax (set-sig-full?-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types sig)])
    #`(: (Any -> Boolean : F)
         #,(set-sig-full? sig))))

(define-for-syntax (set-sig-empty?-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types sig)])
    #`(: (Any -> Boolean : E)
         #,(set-sig-empty? sig))))

(define-for-syntax (set-sig-full-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types sig)])
    #`(: F
         #,(set-sig-full sig))))

(define-for-syntax (set-sig-empty-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types sig)])
    #`(: E
         #,(set-sig-empty sig))))

(define-for-syntax (set-sig-intersect-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types sig)])
    #`(: (case-> ((U N F E) (U N E)   -> (U N E))
                 ((U N E)   (U N F E) -> (U N E))
                 ((U N F E) (U N F E) -> (U N F E)))
         #,(set-sig-intersect sig))))

(define-for-syntax (set-sig-join-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types sig)])
    #`(: (case-> ((U N F E) (U N F)   -> (U N F))
                 ((U N F)   (U N F E) -> (U N F))
                 ((U N F E) (U N F E) -> (U N F E)))
         #,(set-sig-join sig))))

(define-for-syntax (set-sig-subseteq?-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types sig)])
    #`(: ((U N F E) (U N F E) -> Boolean)
         #,(set-sig-subseteq? sig))))

;; ---------------------------------------------------------------------------------------------------
;; Typed binding getters for rects

(define-for-syntax (rect-sig-rect-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types (rect-sig-set-sig sig))]
                [(N1 F1 E1 V1)  (set-sig-types (rect-sig-fst-set-sig sig))]
                [(N2 F2 E2 V2)  (set-sig-types (rect-sig-snd-set-sig sig))])
    #`(: ((U N1 F1) (U N2 F2) -> N)
         #,(rect-sig-rect sig))))

(define-for-syntax (rect-sig-rect-fst-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types (rect-sig-set-sig sig))]
                [(N1 F1 E1 V1)  (set-sig-types (rect-sig-fst-set-sig sig))])
    #`(: (N -> (U N1 F1))
         #,(rect-sig-rect-fst sig))))

(define-for-syntax (rect-sig-rect-snd-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types (rect-sig-set-sig sig))]
                [(N2 F2 E2 V2)  (set-sig-types (rect-sig-snd-set-sig sig))])
    #`(: (N -> (U N2 F2))
         #,(rect-sig-rect-snd sig))))

(define-for-syntax (rect-sig-value-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types (rect-sig-set-sig sig))]
                [(N1 F1 E1 V1)  (set-sig-types (rect-sig-fst-set-sig sig))]
                [(N2 F2 E2 V2)  (set-sig-types (rect-sig-snd-set-sig sig))])
    #`(: (V1 V2 -> V)
         #,(rect-sig-value sig))))

(define-for-syntax (rect-sig-value-fst-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types (rect-sig-set-sig sig))]
                [(N1 F1 E1 V1)  (set-sig-types (rect-sig-fst-set-sig sig))])
    #`(: (V -> V1)
         #,(rect-sig-value-fst sig))))

(define-for-syntax (rect-sig-value-snd-binding sig)
  (with-syntax ([(N F E V)  (set-sig-types (rect-sig-set-sig sig))]
                [(N2 F2 E2 V2)  (set-sig-types (rect-sig-snd-set-sig sig))])
    #`(: (V -> V2)
         #,(rect-sig-value-snd sig))))

;; ===================================================================================================
;; Rectangle operations

(define-for-syntax (value-binding->syntax-binding bnd)
  (syntax-case bnd (:)
    [[name : T  val]
     (syntax/loc bnd
       [name  (make-head-form #'(ann val T))])]
    [[name val]
     (syntax/loc bnd
       [name  (make-head-form #'val)])]))

(define-syntax (let/cbn: stx)
  (syntax-case stx ()
    [(_ (bnds ...) . body)
     (with-syntax ([(bnds ...)  (map value-binding->syntax-binding
                                     (syntax->list #'(bnds ...)))])
       (syntax/loc stx
         (let-syntax (bnds ...) . body)))]))

(define-for-syntax (make-maker make*)
  (λ (sig)
    (define-values (expr type) (make* sig))
    #`(ann #,expr #,type)))

;; ---------------------------------------------------------------------------------------------------
;; Construction

(define-for-syntax (make-rect* sig)
  (define self-sig (rect-sig-set-sig sig))
  (define fst-sig (rect-sig-fst-set-sig sig))
  (define snd-sig (rect-sig-snd-set-sig sig))
  (with-syntax ([(N F E V)  (set-sig-types self-sig)]
                [(N1 F1 E1 V1)  (set-sig-types fst-sig)]
                [(N2 F2 E2 V2)  (set-sig-types snd-sig)])
    (values
     #`(let/cbn:
        ([full . #,(set-sig-full-binding self-sig)]
         [empty . #,(set-sig-empty-binding self-sig)]
         [rect . #,(rect-sig-rect-binding sig)]
         [fst-full? . #,(set-sig-full?-binding fst-sig)]
         [fst-empty? . #,(set-sig-empty?-binding fst-sig)]
         [snd-full? . #,(set-sig-full?-binding snd-sig)]
         [snd-empty? . #,(set-sig-empty?-binding snd-sig)])
        (λ (A B)
          (cond [(and (fst-full? A) (snd-full? B))   full]
                [(fst-empty? A)  empty]
                [(snd-empty? B)  empty]
                [else  (rect A B)])))
     #`(case-> ((U N1 F1) N2 -> N)
               (N1 (U N2 F2) -> N)
               ((U N1 F1) (U N2 F2) -> (U N F))
               ((U N1 E1) (U N2 F2 E2) -> (U N E))
               ((U N1 F1 E1) (U N2 E2) -> (U N E))
               ((U N1 F1 E1) (U N2 F2 E2) -> (U N F E))))))

(define-for-syntax make-rect (make-maker make-rect*))

;; ---------------------------------------------------------------------------------------------------
;; Intersection

(define-for-syntax (make-rect-intersect* sig)
  (define self-sig (rect-sig-set-sig sig))
  (define fst-sig (rect-sig-fst-set-sig sig))
  (define snd-sig (rect-sig-snd-set-sig sig))
  (with-syntax ([(N F E V)  (set-sig-types self-sig)])
    (values
     #`(let/cbn:
        ([full? . #,(set-sig-full?-binding self-sig)]
         [full . #,(set-sig-full-binding self-sig)]
         [empty? . #,(set-sig-empty?-binding self-sig)]
         [empty . #,(set-sig-empty-binding self-sig)]
         [rect . #,(rect-sig-rect-binding sig)]
         [rect-fst . #,(rect-sig-rect-fst-binding sig)]
         [rect-snd . #,(rect-sig-rect-snd-binding sig)]
         [fst-intersect . #,(set-sig-intersect-binding fst-sig)]
         [fst-empty? . #,(set-sig-empty?-binding fst-sig)]
         [snd-intersect . #,(set-sig-intersect-binding snd-sig)]
         [snd-empty? . #,(set-sig-empty?-binding snd-sig)])
        (λ (A B)
          (cond [(full? A)   B]
                [(full? B)   A]
                [(eq? A B)   A]
                [(empty? A)  empty]
                [(empty? B)  empty]
                [else
                 (define A1 (rect-fst A))
                 (define A2 (rect-snd A))
                 (define B1 (rect-fst B))
                 (define B2 (rect-snd B))
                 (define C1 (fst-intersect A1 B1))
                 (cond [(fst-empty? C1)  empty]
                       [else
                        (define C2 (snd-intersect A2 B2))
                        (cond [(snd-empty? C2)  empty]
                              [(and (eq? C1 A1) (eq? C2 A2))  A]
                              [(and (eq? C1 B1) (eq? C2 B2))  B]
                              [else  (rect (if (eq? C1 A1) A1 (if (eq? C1 B1) B1 C1))
                                           (if (eq? C2 A2) A2 (if (eq? C2 B2) B2 C2)))])])])))
     #`(case-> ((U N F E) (U N E)   -> (U N E))
               ((U N E)   (U N F E) -> (U N E))
               ((U N F E) (U N F E) -> (U N F E))))))

(define-for-syntax make-rect-intersect (make-maker make-rect-intersect*))

;; ---------------------------------------------------------------------------------------------------
;; Join

(define-for-syntax (make-rect-join* sig)
  (define self-sig (rect-sig-set-sig sig))
  (define fst-sig (rect-sig-fst-set-sig sig))
  (define snd-sig (rect-sig-snd-set-sig sig))
  (with-syntax ([(N F E V)  (set-sig-types self-sig)])
    (values
     #`(let/cbn:
        ([full? . #,(set-sig-full?-binding self-sig)]
         [full . #,(set-sig-full-binding self-sig)]
         [empty? . #,(set-sig-empty?-binding self-sig)]
         [empty . #,(set-sig-empty-binding self-sig)]
         [rect . #,(rect-sig-rect-binding sig)]
         [rect-fst . #,(rect-sig-rect-fst-binding sig)]
         [rect-snd . #,(rect-sig-rect-snd-binding sig)]
         [fst-join . #,(set-sig-join-binding fst-sig)]
         [fst-full? . #,(set-sig-full?-binding fst-sig)]
         [snd-join . #,(set-sig-join-binding snd-sig)]
         [snd-full? . #,(set-sig-full?-binding snd-sig)])
        (λ (A B)
          (cond [(empty? A)  B]
                [(empty? B)  A]
                [(eq? A B)   A]
                [(full? A)  full]
                [(full? B)  full]
                [else
                 (define A1 (rect-fst A))
                 (define A2 (rect-snd A))
                 (define B1 (rect-fst B))
                 (define B2 (rect-snd B))
                 (define C1 (fst-join A1 B1))
                 (define C2 (snd-join A2 B2))
                 (cond [(and (fst-full? C1) (snd-full? C2))  full]
                       [(and (eq? C1 A1) (eq? C2 A2))  A]
                       [(and (eq? C1 B1) (eq? C2 B2))  B]
                       [else  (rect (if (eq? C1 A1) A1 (if (eq? C1 B1) B1 C1))
                                    (if (eq? C2 A2) A2 (if (eq? C2 B2) B2 C2)))])])))
     #`(case-> ((U N F E) (U N F)   -> (U N F))
               ((U N F)   (U N F E) -> (U N F))
               ((U N F E) (U N F E) -> (U N F E))))))

(define-for-syntax make-rect-join (make-maker make-rect-join*))

;; ---------------------------------------------------------------------------------------------------
;; Membership

(define-for-syntax (make-rect-member?* sig)
  (define self-sig (rect-sig-set-sig sig))
  (define fst-sig (rect-sig-fst-set-sig sig))
  (define snd-sig (rect-sig-snd-set-sig sig))
  (with-syntax ([(N F E V)  (set-sig-types self-sig)])
    (values
     #`(let/cbn:
        ([full? . #,(set-sig-full?-binding self-sig)]
         [empty? . #,(set-sig-empty?-binding self-sig)]
         [rect-fst . #,(rect-sig-rect-fst-binding sig)]
         [rect-snd . #,(rect-sig-rect-snd-binding sig)]
         [fst-member? . #,(set-sig-member?-binding fst-sig)]
         [snd-member? . #,(set-sig-member?-binding snd-sig)]
         [fst . #,(rect-sig-value-fst-binding sig)]
         [snd . #,(rect-sig-value-snd-binding sig)])
        (λ (A x)
          (cond [(full? A)  #t]
                [(empty? A)  #f]
                [else  (and (fst-member? (rect-fst A) (fst x))
                            (snd-member? (rect-snd A) (snd x)))])))
     #`((U N F E) V -> Boolean))))

(define-for-syntax make-rect-member? (make-maker make-rect-member?*))

;; ---------------------------------------------------------------------------------------------------
;; Subset or equal

(define-for-syntax (make-rect-subseteq?* sig)
  (define self-sig (rect-sig-set-sig sig))
  (define fst-sig (rect-sig-fst-set-sig sig))
  (define snd-sig (rect-sig-snd-set-sig sig))
  (with-syntax ([(N F E V)  (set-sig-types self-sig)])
    (values
     #`(let/cbn:
        ([full? . #,(set-sig-full?-binding self-sig)]
         [empty? . #,(set-sig-empty?-binding self-sig)]
         [rect-fst . #,(rect-sig-rect-fst-binding sig)]
         [rect-snd . #,(rect-sig-rect-snd-binding sig)]
         [fst-subseteq? . #,(set-sig-subseteq?-binding fst-sig)]
         [snd-subseteq? . #,(set-sig-subseteq?-binding snd-sig)])
        (λ (A B)
          (cond [(eq? A B)  #t]
                [(empty? A)  #t]
                [(empty? B)  #f]
                [(full? B)   #t]
                [(full? A)   #f]
                [else  (and (fst-subseteq? (rect-fst A) (rect-fst B))
                            (snd-subseteq? (rect-snd A) (rect-snd B)))])))
     #`((U N F E) (U N F E) -> Boolean))))

(define-for-syntax make-rect-subseteq? (make-maker make-rect-subseteq?*))

;; ===================================================================================================
;; Instantiation

(define-syntax (define-rect-constructor stx)
  (syntax-case stx ()
    [(_ name sig)
     (let-values ([(expr type)  (make-rect* (syntax-local-value #'sig))])
       (quasisyntax/loc stx
         (begin (: name #,type)
                (define name #,expr))))]))

(define-syntax (define-rect-ops stx)
  (syntax-case stx ()
    [(_ rect sig)
     (let ([sig  (syntax-local-value #'sig)])
       (with-syntax ([member?    (format-id stx "~a-~a" #'rect 'member?)]
                     [intersect  (format-id stx "~a-~a" #'rect 'intersect)]
                     [join       (format-id stx "~a-~a" #'rect 'join)]
                     [subseteq?  (format-id stx "~a-~a" #'rect 'subseteq?)])
         (let-values ([(member?-expr    member?-type)    (make-rect-member?* sig)]
                      [(intersect-expr  intersect-type)  (make-rect-intersect* sig)]
                      [(join-expr       join-type)       (make-rect-join* sig)]
                      [(subseteq?-expr  subseteq?-type)  (make-rect-subseteq?* sig)])
           (quasisyntax/loc stx
             (begin
               (: member? #,member?-type)
               (define member? #,member?-expr)
               
               (: intersect #,intersect-type)
               (define intersect #,intersect-expr)
               
               (: join #,join-type)
               (define join #,join-expr)
               
               (: subseteq? #,subseteq?-type)
               (define subseteq? #,subseteq?-expr))))))]))
