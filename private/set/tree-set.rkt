#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         racket/list
         racket/promise
         math/flonum
         "types.rkt"
         "extremal-set.rkt"
         "real-set.rkt"
         "bool-set.rkt"
         "pair-set.rkt"
         "tree-value.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Omega signatures and operations

(struct: Base-Omega-Set Base-Bot-Basic () #:transparent)
(define omega-set? Base-Omega-Set?)

(define-singleton-type Empty-Omega-Set Base-Omega-Set empty-omega-set)
(define-singleton-type Full-Omega-Set Base-Omega-Set omegas)
(struct: Nonextremal-Omega-Set Base-Omega-Set
  ([values : Nonempty-Real-Set] [children : Nonempty-Omega-Children-Set])
  #:transparent)

(define-type Nonfull-Omega-Set (U Empty-Omega-Set Nonextremal-Omega-Set))
(define-type Nonempty-Omega-Set (U Full-Omega-Set Nonextremal-Omega-Set))
(define-type Omega-Set (U Empty-Omega-Set Full-Omega-Set Nonextremal-Omega-Set))

(define-singleton-type Empty-Omega-Children-Set empty-omega-children-set)
(define-singleton-type Full-Omega-Children-Set full-omega-children-set)
(struct: Omega-Children-Rect ([fst : Nonempty-Omega-Set] [snd : Nonempty-Omega-Set]) #:transparent)

(define-type Nonfull-Omega-Children-Set (U Empty-Omega-Children-Set Omega-Children-Rect))
(define-type Nonempty-Omega-Children-Set (U Full-Omega-Children-Set Omega-Children-Rect))
(define-type Omega-Children-Set
  (U Empty-Omega-Children-Set Full-Omega-Children-Set Omega-Children-Rect))

(define omega-rect Nonextremal-Omega-Set)

(define-syntax real-set-sig
  (set-sig
   #'(Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum)
   #'real-set-member?
   #'reals?
   #'empty-real-set?
   #'reals
   #'empty-real-set
   #'real-set-intersect
   #'real-set-union
   #'real-set-subseteq?))

(define-syntax omega-set-sig
  (set-sig
   #'(Nonextremal-Omega-Set Full-Omega-Set Empty-Omega-Set Omega)
   #'omega-set-member?
   #'omegas?
   #'empty-omega-set?
   #'omegas
   #'empty-omega-set
   #'omega-set-intersect
   #'omega-set-join
   #'omega-set-subseteq?))

(define-syntax omega-children-set-sig
  (set-sig
   #'(Omega-Children-Rect Full-Omega-Children-Set Empty-Omega-Children-Set (Children Flonum))
   #'omega-children-set-member?
   #'full-omega-children-set?
   #'empty-omega-children-set?
   #'full-omega-children-set
   #'empty-omega-children-set
   #'omega-children-set-intersect
   #'omega-children-set-join
   #'omega-children-set-subseteq?))

(define-syntax omega-rect-sig
  (rect-sig
   (syntax-local-value #'omega-set-sig)
   (syntax-local-value #'real-set-sig)
   (syntax-local-value #'omega-children-set-sig)
   #'omega-rect #'Nonextremal-Omega-Set-values #'Nonextremal-Omega-Set-children
   #'make-omega #'omega-value #'omega-children))

(define-syntax omega-children-rect-sig
  (rect-sig
   (syntax-local-value #'omega-children-set-sig)
   (syntax-local-value #'omega-set-sig)
   (syntax-local-value #'omega-set-sig)
   #'Omega-Children-Rect #'Omega-Children-Rect-fst #'Omega-Children-Rect-snd
   #'make-omega-children #'omega-children-fst #'omega-children-snd))

(define-rect-constructor omega-set omega-rect-sig)
(define-rect-ops omega-set omega-rect-sig)

(define-rect-constructor omega-children-set omega-children-rect-sig)
(define-rect-ops omega-children-set omega-children-rect-sig)

;; ===================================================================================================
;; Trace signatures and operations

(struct: Base-Trace-Set Base-Bot-Basic () #:transparent)
(define trace-set? Base-Trace-Set?)

(define-singleton-type Empty-Trace-Set Base-Trace-Set empty-trace-set)
(define-singleton-type Full-Trace-Set Base-Trace-Set traces)
(struct: Nonextremal-Trace-Set Base-Trace-Set
  ([values : Nonempty-Bool-Set] [children : Nonempty-Trace-Children-Set])
  #:transparent)

(define-type Nonfull-Trace-Set (U Empty-Trace-Set Nonextremal-Trace-Set))
(define-type Nonempty-Trace-Set (U Full-Trace-Set Nonextremal-Trace-Set))
(define-type Trace-Set (U Empty-Trace-Set Full-Trace-Set Nonextremal-Trace-Set))

(define-singleton-type Empty-Trace-Children-Set empty-trace-children-set)
(define-singleton-type Full-Trace-Children-Set full-trace-children-set)
(struct: Trace-Children-Rect ([fst : Nonempty-Trace-Set] [snd : Nonempty-Trace-Set]) #:transparent)

(define-type Nonfull-Trace-Children-Set (U Empty-Trace-Children-Set Trace-Children-Rect))
(define-type Nonempty-Trace-Children-Set (U Full-Trace-Children-Set Trace-Children-Rect))
(define-type Trace-Children-Set
  (U Empty-Trace-Children-Set Full-Trace-Children-Set Trace-Children-Rect))

(define trace-rect Nonextremal-Trace-Set)

(define-syntax bool-set-sig
  (set-sig
   #'(Nonextremal-Bool-Set Full-Bool-Set Empty-Bool-Set Boolean)
   #'bool-set-member?
   #'bools?
   #'empty-bool-set?
   #'bools
   #'empty-bool-set
   #'bool-set-intersect
   #'bool-set-union
   #'bool-set-subseteq?))

(define-syntax trace-set-sig
  (set-sig
   #'(Nonextremal-Trace-Set Full-Trace-Set Empty-Trace-Set Trace)
   #'trace-set-member?
   #'traces?
   #'empty-trace-set?
   #'traces
   #'empty-trace-set
   #'trace-set-intersect
   #'trace-set-join
   #'trace-set-subseteq?))

(define-syntax trace-children-set-sig
  (set-sig
   #'(Trace-Children-Rect Full-Trace-Children-Set Empty-Trace-Children-Set (Children Boolean))
   #'trace-children-set-member?
   #'full-trace-children-set?
   #'empty-trace-children-set?
   #'full-trace-children-set
   #'empty-trace-children-set
   #'trace-children-set-intersect
   #'trace-children-set-join
   #'trace-children-set-subseteq?))

(define-syntax trace-rect-sig
  (rect-sig
   (syntax-local-value #'trace-set-sig)
   (syntax-local-value #'bool-set-sig)
   (syntax-local-value #'trace-children-set-sig)
   #'trace-rect #'Nonextremal-Trace-Set-values #'Nonextremal-Trace-Set-children
   #'make-trace #'trace-value #'trace-children))

(define-syntax trace-children-rect-sig
  (rect-sig
   (syntax-local-value #'trace-children-set-sig)
   (syntax-local-value #'trace-set-sig)
   (syntax-local-value #'trace-set-sig)
   #'Trace-Children-Rect #'Trace-Children-Rect-fst #'Trace-Children-Rect-snd
   #'make-trace-children #'trace-children-fst #'trace-children-snd))

(define-rect-constructor trace-set trace-rect-sig)
(define-rect-ops trace-set trace-rect-sig)

(define-rect-constructor trace-children-set trace-children-rect-sig)
(define-rect-ops trace-children-set trace-children-rect-sig)

;; ===================================================================================================
;; Projections

(define-for-syntax (make-tree-set-proj tree-rect-sig child-rect-sig)
  (define tree-sig (rect-sig-set-sig tree-rect-sig))
  (define proj-sig (rect-sig-fst-set-sig tree-rect-sig))
  (define child-sig (rect-sig-snd-set-sig tree-rect-sig))
  (with-syntax ([(TN TF TE TV)  (set-sig-types tree-sig)]
                [(PN PF PE PV)  (set-sig-types proj-sig)]
                [(CN CF CE CV)  (set-sig-types child-sig)])
    (values
     #`(let/cbn:
        ([tree-set-full? . #,(set-sig-full?-binding tree-sig)]
         [tree-set-empty? . #,(set-sig-empty?-binding tree-sig)]
         [tree-set-fst . #,(rect-sig-rect-fst-binding tree-rect-sig)]
         [tree-set-snd . #,(rect-sig-rect-snd-binding tree-rect-sig)]
         [child-set-full? . #,(set-sig-full?-binding child-sig)]
         [child-set-fst . #,(rect-sig-rect-fst-binding child-rect-sig)]
         [child-set-snd . #,(rect-sig-rect-snd-binding child-rect-sig)]
         [full-proj-set . #,(set-sig-full-binding proj-sig)]
         [empty-proj-set . #,(set-sig-empty-binding proj-sig)])
        (: tree-set-proj (case-> (TF Tree-Index -> PF)
                                 (TE Tree-Index -> PE)
                                 ((U TN TF) Tree-Index -> (U PN PF))
                                 ((U TN TE TF) Tree-Index -> (U PN PE PF))))
        (define (tree-set-proj A j)
          (cond [(tree-set-empty? A)  empty-proj-set]
                [else  (tree-set-proj* A j)]))
        
        (: tree-set-proj* (case-> (TF Tree-Index -> PF)
                                  ((U TN TF) Tree-Index -> (U PN PF))))
        (define (tree-set-proj* A j)
          (cond [(tree-set-full? A)  full-proj-set]
                [(empty? j)  (tree-set-fst A)]
                [else  (tree-children-set-proj* (tree-set-snd A) j)]))
        
        (: tree-children-set-proj* (case-> (CF Nonempty-Tree-Index -> PF)
                                           ((U CN CF) Nonempty-Tree-Index -> (U PN PF))))
        (define (tree-children-set-proj* A j)
          (cond [(child-set-full? A)  full-proj-set]
                [(zero? (first j))  (tree-set-proj* (child-set-fst A) (rest j))]
                [else               (tree-set-proj* (child-set-snd A) (rest j))]))
        
        tree-set-proj)
     #`(case-> (TE Tree-Index -> PE)
               (TF Tree-Index -> PF)
               ((U TN TF) Tree-Index -> (U PN PF))
               ((U TN TE TF) Tree-Index -> (U PN PE PF))))))

(define-syntax (define-tree-set-proj stx)
  (syntax-case stx ()
    [(_ name tree-sig child-sig)
     (let ([tree-sig  (syntax-local-value #'tree-sig)]
           [child-sig  (syntax-local-value #'child-sig)])
       (let-values ([(expr type)  (make-tree-set-proj tree-sig child-sig)])
         (quasisyntax/loc stx
           (begin
             (: name #,type)
             (define name #,expr)))))]))

(define-tree-set-proj omega-set-proj* omega-rect-sig omega-children-rect-sig)

(: omega-set-proj (case-> (Empty-Omega-Set Tree-Index -> Empty-Real-Set)
                          (Full-Omega-Set Tree-Index -> Nonextremal-Real-Set)
                          (Nonempty-Omega-Set Tree-Index -> Nonempty-Real-Set)
                          (Omega-Set Tree-Index -> Real-Set)))
(define (omega-set-proj A j)
  (cond [(empty-omega-set? A)  empty-real-set]
        [(omegas? A)   unit-interval]
        [else
         (define Aj (omega-set-proj* A j))
         (define B (real-set-intersect Aj unit-interval))
         (cond [(empty-real-set? B)
                (error 'omega-set-proj "projection at ~e does not intersect unit-interval: ~e" j Aj)]
               [else  B])]))

(define-tree-set-proj trace-set-proj trace-rect-sig trace-children-rect-sig)

;; ===================================================================================================
;; Projection preimages

(define-for-syntax (make-tree-set-unproj tree-rect-sig child-rect-sig)
  (define tree-sig (rect-sig-set-sig tree-rect-sig))
  (define proj-sig (rect-sig-fst-set-sig tree-rect-sig))
  (define child-sig (rect-sig-snd-set-sig tree-rect-sig))
  (with-syntax ([(TN TF TE TV)  (set-sig-types tree-sig)]
                [(PN PF PE PV)  (set-sig-types proj-sig)]
                [(CN CF CE CV)  (set-sig-types child-sig)])
    (values
     #`(let/cbn:
        ([tree-set-full? . #,(set-sig-full?-binding tree-sig)]
         [tree-set-empty? . #,(set-sig-empty?-binding tree-sig)]
         [full-tree-set . #,(set-sig-full-binding tree-sig)]
         [empty-tree-set . #,(set-sig-empty-binding tree-sig)]
         [make-tree-set . #,(rect-sig-rect-binding tree-rect-sig)]
         [tree-set-fst . #,(rect-sig-rect-fst-binding tree-rect-sig)]
         [tree-set-snd . #,(rect-sig-rect-snd-binding tree-rect-sig)]
         [child-set-full? . #,(set-sig-full?-binding child-sig)]
         [child-set-empty? . #,(set-sig-empty?-binding child-sig)]
         [full-child-set . #,(set-sig-full-binding child-sig)]
         [empty-child-set . #,(set-sig-empty-binding child-sig)]
         [make-child-set . #,(rect-sig-rect-binding child-rect-sig)]
         [child-set-fst . #,(rect-sig-rect-fst-binding child-rect-sig)]
         [child-set-snd . #,(rect-sig-rect-snd-binding child-rect-sig)]
         [proj-set-full? . #,(set-sig-full?-binding proj-sig)]
         [proj-set-empty? . #,(set-sig-empty?-binding proj-sig)]
         [full-proj-set . #,(set-sig-full-binding proj-sig)]
         [proj-set-intersect . #,(set-sig-intersect-binding proj-sig)])
        (: tree-set-unproj (case-> (TE Tree-Index (U PN PF PE) -> TE)
                                   ((U TN TF TE) Tree-Index PE -> TE)
                                   (TF Tree-Index PF -> TF)
                                   (TF Tree-Index (U PN PF) -> (U TN TF))
                                   ((U TN TF) Tree-Index PF -> (U TN TF))
                                   ((U TN TE) Tree-Index (U PN PF PE) -> (U TN TE))
                                   ((U TN TF TE) Tree-Index (U PN PE) -> (U TN TE))
                                   ((U TN TF TE) Tree-Index (U PN PF PE) -> (U TN TF TE))))
        (define (tree-set-unproj A j B)
          (cond [(tree-set-empty? A)  empty-tree-set]
                [(proj-set-empty? B)  empty-tree-set]
                [(proj-set-full? B)   A]
                [else  (tree-set-unproj* A j B)]))
        
        (: full-tree-set-unproj* (Tree-Index PN -> TN))
        (define (full-tree-set-unproj* j B)
          (cond [(empty? j)
                 (make-tree-set B full-child-set)]
                [else
                 (make-tree-set full-proj-set (full-child-set-unproj* j B))]))
        
        (: full-child-set-unproj* (Nonempty-Tree-Index PN -> CN))
        (define (full-child-set-unproj* j B)
          (cond [(zero? (first j))
                 (make-child-set (full-tree-set-unproj* (rest j) B) full-tree-set)]
                [else
                 (make-child-set full-tree-set (full-tree-set-unproj* (rest j) B))]))
        
        (: tree-set-unproj* (case-> (TF Tree-Index PN -> TN)
                                    ((U TN TF) Tree-Index PN -> (U TN TE))))
        (define (tree-set-unproj* A j B)
          (cond [(tree-set-full? A)   (full-tree-set-unproj* j B)]
                [(empty? j)
                 (define A1 (proj-set-intersect (tree-set-fst A) B))
                 (if (proj-set-empty? A1) empty-tree-set (make-tree-set A1 (tree-set-snd A)))]
                [else
                 (define A2 (child-set-unproj* (tree-set-snd A) j B))
                 (if (child-set-empty? A2) empty-tree-set (make-tree-set (tree-set-fst A) A2))]))
        
        (: child-set-unproj* (case-> (CF Nonempty-Tree-Index PN -> CN)
                                     ((U CF CN) Nonempty-Tree-Index PN -> (U CN CE))))
        (define (child-set-unproj* A j B)
          (cond [(child-set-full? A)  (full-child-set-unproj* j B)]
                [(zero? (first j))
                 (define A1 (tree-set-unproj* (child-set-fst A) (rest j) B))
                 (if (tree-set-empty? A1) empty-child-set (make-child-set A1 (child-set-snd A)))]
                [else
                 (define A2 (tree-set-unproj* (child-set-snd A) (rest j) B))
                 (if (tree-set-empty? A2) empty-child-set (make-child-set (child-set-fst A) A2))]))
        
        tree-set-unproj)
     
     #`(case-> (TE Tree-Index (U PN PF PE) -> TE)
               ((U TN TF TE) Tree-Index PE -> TE)
               (TF Tree-Index PF -> TF)
               (TF Tree-Index (U PN PF) -> (U TN TF))
               ((U TN TF) Tree-Index PF -> (U TN TF))
               ((U TN TE) Tree-Index (U PN PF PE) -> (U TN TE))
               ((U TN TF TE) Tree-Index (U PN PE) -> (U TN TE))
               ((U TN TF TE) Tree-Index (U PN PF PE) -> (U TN TF TE))))))

(define-syntax (define-tree-set-unproj stx)
  (syntax-case stx ()
    [(_ name tree-sig child-sig)
     (let ([tree-sig  (syntax-local-value #'tree-sig)]
           [child-sig  (syntax-local-value #'child-sig)])
       (let-values ([(expr type)  (make-tree-set-unproj tree-sig child-sig)])
         (quasisyntax/loc stx
           (begin
             (: name #,type)
             (define name #,expr)))))]))

(define-tree-set-unproj omega-set-unproj* omega-rect-sig omega-children-rect-sig)
(define-tree-set-unproj trace-set-unproj trace-rect-sig trace-children-rect-sig)

(: omega-set-unproj (case-> (Empty-Omega-Set Tree-Index Real-Set -> Empty-Omega-Set)
                            (Omega-Set Tree-Index Empty-Real-Set -> Empty-Omega-Set)
                            (Full-Omega-Set Tree-Index Full-Real-Set -> Full-Omega-Set)
                            (Full-Omega-Set Tree-Index Nonempty-Real-Set -> Nonempty-Omega-Set)
                            (Nonempty-Omega-Set Tree-Index Full-Real-Set -> Nonempty-Omega-Set)
                            (Nonfull-Omega-Set Tree-Index Real-Set -> Nonfull-Omega-Set)
                            (Omega-Set Tree-Index Nonfull-Real-Set -> Nonfull-Omega-Set)
                            (Omega-Set Tree-Index Real-Set -> Omega-Set)))
(define (omega-set-unproj A j B)
  (if (empty-real-set? (real-set-intersect B unit-interval))
      (raise-argument-error 'omega-set-unproj "real set intersecting unit-interval" 2 A j B)
      (omega-set-unproj* A j B)))
#|
;; ===================================================================================================
;; Singletons

(define-for-syntax (make-tree->singleton tree-rect-sig child-rect-sig any-tree? proj->singleton)
  (define tree-sig (rect-sig-set-sig tree-rect-sig))
  (define proj-sig (rect-sig-fst-set-sig tree-rect-sig))
  (define child-sig (rect-sig-snd-set-sig tree-rect-sig))
  (with-syntax ([(TN TF TE TV)  (set-sig-types tree-sig)]
                [(PN PF PE PV)  (set-sig-types proj-sig)]
                [(CN CF CE CV)  (set-sig-types child-sig)])
    (values
     #`(let/cbn:
        ([full-tree-set . #,(set-sig-full-binding tree-sig)]
         [tree-fst . #,(rect-sig-value-fst-binding tree-rect-sig)]
         [tree-snd . #,(rect-sig-value-snd-binding tree-rect-sig)]
         [make-tree-set . #,(rect-sig-rect-binding tree-rect-sig)]
         [child-fst . #,(rect-sig-value-fst-binding child-rect-sig)]
         [child-snd . #,(rect-sig-value-snd-binding child-rect-sig)]
         [make-child-set . #,(rect-sig-rect-binding child-rect-sig)]
         [any-tree? : (TV -> Boolean)  #,any-tree?]
         [proj->singleton : (PV -> (U PN PF))  #,proj->singleton])
        
        (: tree->singleton (TV -> (U TN TF)))
        (define (tree->singleton t)
          (cond [(any-tree? t)  full-tree-set]
                [else  (make-tree-set (proj->singleton (tree-fst t))
                                      (child->singleton (tree-snd t)))]))
        
        (: child->singleton (CV -> CN))
        (define (child->singleton c)
          (make-child-set (tree->singleton (child-fst c))
                          (tree->singleton (child-snd c))))
        
        tree->singleton)
     
     #`(TV -> (U TN TF)))))

(define-syntax (define-tree->singleton stx)
  (syntax-case stx ()
    [(_ name tree-sig child-sig any-tree? proj->singleton)
     (let ([tree-sig  (syntax-local-value #'tree-sig)]
           [child-sig  (syntax-local-value #'child-sig)])
       (let-values ([(expr type)  (make-tree->singleton tree-sig child-sig
                                                        #'any-tree? #'proj->singleton)])
         (quasisyntax/loc stx
           (begin
             (: name #,type)
             (define name #,expr)))))]))

(: flonum->singleton* (Flonum -> Nonempty-Real-Set))
(define (flonum->singleton* x)
  (cond [(<= 0.0 x 1.0)  (flonum->singleton x)]
        [else  (raise-argument-error 'flonum->singleton* "number in unit-interval" x)]))

(define-tree->singleton omega->singleton
  omega-rect-sig omega-children-rect-sig any-omega? flonum->singleton*)

(define-tree->singleton trace->singleton
  trace-rect-sig trace-children-rect-sig any-trace? boolean->singleton)
|#
;; ===================================================================================================
;; Sampling

(define-for-syntax (make-tree-set-sample tree-rect-sig child-rect-sig proj-set-sample random-tree)
  (define tree-sig (rect-sig-set-sig tree-rect-sig))
  (define proj-sig (rect-sig-fst-set-sig tree-rect-sig))
  (define child-sig (rect-sig-snd-set-sig tree-rect-sig))
  (with-syntax ([(TN TF TE TV)  (set-sig-types tree-sig)]
                [(PN PF PE PV)  (set-sig-types proj-sig)]
                [(CN CF CE CV)  (set-sig-types child-sig)])
    (values
     #`(let/cbn:
        ([tree-set-full? . #,(set-sig-full?-binding tree-sig)]
         [tree-set-fst . #,(rect-sig-rect-fst-binding tree-rect-sig)]
         [tree-set-snd . #,(rect-sig-rect-snd-binding tree-rect-sig)]
         [make-tree-value . #,(rect-sig-value-binding tree-rect-sig)]
         [child-set-full? . #,(set-sig-full?-binding child-sig)]
         [child-set-fst . #,(rect-sig-rect-fst-binding child-rect-sig)]
         [child-set-snd . #,(rect-sig-rect-snd-binding child-rect-sig)]
         [make-child-value . #,(rect-sig-value-binding child-rect-sig)]
         [proj-set-sample : ((U PN PF) -> PV) #,proj-set-sample]
         [random-tree : (-> TV)  #,random-tree])
        
        (: tree-set-sample ((U TN TF) -> TV))
        (define (tree-set-sample A)
          (cond [(tree-set-full? A)  (random-tree)]
                [else  (make-tree-value (proj-set-sample (tree-set-fst A))
                                        (child-set-sample (tree-set-snd A)))]))
        
        (: child-set-sample ((U CN CF) -> CV))
        (define (child-set-sample A)
          (cond [(child-set-full? A)  (make-child-value (random-tree) (random-tree))]
                [else  (make-child-value (tree-set-sample (child-set-fst A))
                                         (tree-set-sample (child-set-snd A)))]))
        
        tree-set-sample)
     
     #`((U TN TF) -> TV))))

(define-syntax (define-tree-set-sample stx)
  (syntax-case stx ()
    [(_ name tree-sig child-sig proj-set-sample any-tree-value)
     (let ([tree-sig  (syntax-local-value #'tree-sig)]
           [child-sig  (syntax-local-value #'child-sig)])
       (let-values ([(expr type)  (make-tree-set-sample tree-sig child-sig
                                                        #'proj-set-sample #'any-tree-value)])
         (quasisyntax/loc stx
           (begin
             (: name #,type)
             (define name #,expr)))))]))

(: real-set-unit-sample (Nonempty-Real-Set -> Flonum))
(define (real-set-unit-sample A)
  (cond [(reals? A)  +nan.0]
        [else
         (define B (real-set-intersect A unit-interval))
         (cond [(empty-real-set? B)
                (raise-argument-error 'real-set-unit-sample "real set intersecting unit-interval" A)]
               [else
                (real-set-sample-point B)])]))

(: bool-set-sample (Nonempty-Bool-Set -> Boolean))
(define (bool-set-sample A)
  (cond [(bools? A)  ((random) . < . 0.5)]
        [(trues? A)  #t]
        [(falses? A)  #f]))

(define-tree-set-sample omega-set-sample-point
  omega-rect-sig omega-children-rect-sig real-set-unit-sample random-omega)

(define-tree-set-sample trace-set-sample-point
  trace-rect-sig trace-children-rect-sig bool-set-sample random-trace)

;; ===================================================================================================
;; Measurement

(: omega-set-measure (Omega-Set -> Flonum))
(define (omega-set-measure A)
  (cond [(omegas? A)  1.0]
        [(empty-omega-set? A)  0.0]
        [else  (match-define (Nonextremal-Omega-Set A1 A2) A)
               (define B (real-set-intersect A1 unit-interval))
               (cond [(empty-real-set? B)
                      (error 'omega-set-measure "projection does not intersect unit-interval: ~e" A1)]
                     [else
                      (fl* (real-set-measure B) (omega-children-set-measure A2))])]))

(: omega-children-set-measure (Omega-Children-Set -> Flonum))
(define (omega-children-set-measure A)
  (cond [(full-omega-children-set? A)  1.0]
        [(empty-omega-children-set? A)  0.0]
        [else  (match-define (Omega-Children-Rect A1 A2) A)
               (fl* (omega-set-measure A1) (omega-set-measure A2))]))

;; ===================================================================================================
;; Conversion

(define-for-syntax (make-tree-set->list tree-rect-sig child-rect-sig)
  (define tree-sig (rect-sig-set-sig tree-rect-sig))
  (define proj-sig (rect-sig-fst-set-sig tree-rect-sig))
  (define child-sig (rect-sig-snd-set-sig tree-rect-sig))
  (with-syntax ([(TN TF TE TV)  (set-sig-types tree-sig)]
                [(PN PF PE PV)  (set-sig-types proj-sig)]
                [(CN CF CE CV)  (set-sig-types child-sig)])
    (values
     #`(let/cbn:
        ([tree-set-full? . #,(set-sig-full?-binding tree-sig)]
         [tree-set-empty? . #,(set-sig-empty?-binding tree-sig)]
         [tree-set-fst . #,(rect-sig-rect-fst-binding tree-rect-sig)]
         [tree-set-snd . #,(rect-sig-rect-snd-binding tree-rect-sig)]
         [child-set-full? . #,(set-sig-full?-binding child-sig)]
         [child-set-fst . #,(rect-sig-rect-fst-binding child-rect-sig)]
         [child-set-snd . #,(rect-sig-rect-snd-binding child-rect-sig)]
         [proj-set-full? . #,(set-sig-full?-binding proj-sig)])
        (λ (A)
          (cond [(tree-set-empty? A)  empty]
                [else
                 (reverse
                  (let: loop ([A A] [As : (Listof (U PN PF))  empty])
                    (cond [(tree-set-full? A)  As]
                          [else
                           (define A1 (tree-set-fst A))
                           (define A2 (tree-set-snd A))
                           (cond [(child-set-full? A2)
                                  (cond [(proj-set-full? A1)  As]
                                        [else  (cons A1 As)])]
                                 [else
                                  (define B1 (child-set-fst A2))
                                  (define B2 (child-set-snd A2))
                                  (let ([As  (loop B1 As)])
                                    (cond [(proj-set-full? A1)  (loop B2 As)]
                                          [else  (loop B2 (cons A1 As))]))])])))])))
     #`((U TN TF TE) -> (Listof (U PN PF))))))

(define-syntax (define-tree-set->list stx)
  (syntax-case stx ()
    [(_ name tree-sig child-sig)
     (let ([tree-sig  (syntax-local-value #'tree-sig)]
           [child-sig  (syntax-local-value #'child-sig)])
       (let-values ([(expr type)  (make-tree-set->list tree-sig child-sig)])
         (quasisyntax/loc stx
           (begin
             (: name #,type)
             (define name #,expr)))))]))

(define-tree-set->list omega-set->list* omega-rect-sig omega-children-rect-sig)
(define-tree-set->list trace-set->list trace-rect-sig trace-children-rect-sig)

(: omega-set->list (Omega-Set -> (Listof Nonempty-Real-Set)))
(define (omega-set->list A)
  (map (λ: ([B : Nonempty-Real-Set])
         (define C (real-set-intersect B unit-interval))
         (cond [(empty-real-set? C)
                (error 'omega-set->list "projection does not intersect unit-interval: ~e" B)]
               [else  C]))
       (omega-set->list* A)))
