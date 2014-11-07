#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         racket/list
         racket/promise
         math/flonum
         "types.rkt"
         "bottom.rkt"
         "extremal-set.rkt"
         "real-set.rkt"
         "bool-set.rkt"
         "indexed.rkt"
         "tree-value.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Omega sets

(struct: Base-Omega-Set Base-Bot-Basic () #:transparent)
(define omega-set? Base-Omega-Set?)

(define-singleton-type Empty-Omega-Set Base-Omega-Set empty-omega-set)
(define-singleton-type Full-Omega-Set Base-Omega-Set omegas)
(struct Nonextremal-Omega-Set Base-Omega-Set ([rect : (Indexed-Rect Nonextremal-Real-Set)])
  #:transparent)

(define-type Nonfull-Omega-Set (U Empty-Omega-Set Nonextremal-Omega-Set))
(define-type Nonempty-Omega-Set (U Full-Omega-Set Nonextremal-Omega-Set))
(define-type Omega-Set (U Empty-Omega-Set Full-Omega-Set Nonextremal-Omega-Set))

(: omega-set-axis (-> Omega-Set Real-Set))
(define (omega-set-axis Ω)
  (cond [(empty-omega-set? Ω)  empty-real-set]
        [(omegas? Ω)  reals]
        [else  (define B (indexed-rect-value (Nonextremal-Omega-Set-rect Ω)))
               (if (true? B) reals B)]))

(: omega-set-left (-> Omega-Set Omega-Set))
(define (omega-set-left Ω)
  (cond [(empty-omega-set? Ω)  empty-omega-set]
        [(omegas? Ω)  omegas]
        [else  (let ([t  (indexed-rect-left (Nonextremal-Omega-Set-rect Ω))])
                 (if (true? t) omegas (Nonextremal-Omega-Set t)))]))

(: omega-set-right (-> Omega-Set Omega-Set))
(define (omega-set-right Ω)
  (cond [(empty-omega-set? Ω)  empty-omega-set]
        [(omegas? Ω)  omegas]
        [else  (let ([t  (indexed-rect-right (Nonextremal-Omega-Set-rect Ω))])
                 (if (true? t) omegas (Nonextremal-Omega-Set t)))]))

(: omega-set (-> Real-Set Omega-Set Omega-Set Omega-Set))
(define (omega-set B Ω1 Ω2)
  (cond [(or (empty-real-set? B) (empty-omega-set? Ω1) (empty-omega-set? Ω2))  empty-omega-set]
        [(and (reals? B) (omegas? Ω1) (omegas? Ω2))  omegas]
        [else  (Nonextremal-Omega-Set ((inst Indexed-Rect Nonextremal-Real-Set)
                                       (if (reals? B) #t B)
                                       (if (omegas? Ω1) #t (Nonextremal-Omega-Set-rect Ω1))
                                       (if (omegas? Ω2) #t (Nonextremal-Omega-Set-rect Ω2))))]))

(: omega-set-projs (-> Omega-Set (Values Real-Set Omega-Set Omega-Set)))
(define (omega-set-projs Ω)
  (cond [(empty-omega-set? Ω)  (values empty-real-set empty-omega-set empty-omega-set)]
        [(omegas? Ω)  (values reals omegas omegas)]
        [else  (match-define (Indexed-Rect B Ω1 Ω2) (Nonextremal-Omega-Set-rect Ω))
               (values (if (true? B) reals B)
                       (if (true? Ω1) omegas (Nonextremal-Omega-Set Ω1))
                       (if (true? Ω2) omegas (Nonextremal-Omega-Set Ω2)))]))

(: omega-set-unaxis (-> Omega-Set Real-Set Omega-Set))
(define (omega-set-unaxis Ω B)
  (cond [(or (empty-omega-set? Ω) (empty-real-set? B))  empty-omega-set]
        [else
         (define-values (B* Ω1 Ω2) (omega-set-projs Ω))
         (omega-set (real-set-intersect B* B) Ω1 Ω2)]))

(: omega-set-unleft (-> Omega-Set Omega-Set Omega-Set))
(define (omega-set-unleft Ω Ω1)
  (cond [(or (empty-omega-set? Ω) (empty-omega-set? Ω1))  empty-omega-set]
        [else
         (define-values (B Ω1* Ω2) (omega-set-projs Ω))
         (omega-set B Ω1 #;(omega-set-intersect Ω1* Ω1) Ω2)]))

(: omega-set-unright (-> Omega-Set Omega-Set Omega-Set))
(define (omega-set-unright Ω Ω2)
  (cond [(or (empty-omega-set? Ω) (empty-omega-set? Ω2))  empty-omega-set]
        [else
         (define-values (B Ω1 Ω2*) (omega-set-projs Ω))
         (omega-set B Ω1 Ω2 #;(omega-set-intersect Ω2* Ω2))]))

(: omega-set-member? (-> Omega-Set Omega Boolean))
(define (omega-set-member? Ω ω)
  (cond [(empty-omega-set? Ω)  #f]
        [(omegas? Ω)  #t]
        [else  (indexed-rect-member? real-set-member?
                                     (Nonextremal-Omega-Set-rect Ω)
                                     (Omega-value ω))]))

(: omega-set-intersect (case-> (-> Omega-Set Nonfull-Omega-Set Nonfull-Omega-Set)
                               (-> Nonfull-Omega-Set Omega-Set Nonfull-Omega-Set)
                               (-> Omega-Set Omega-Set Omega-Set)))
(define (omega-set-intersect Ω1 Ω2)
  (cond [(empty-omega-set? Ω1)  Ω1]
        [(empty-omega-set? Ω2)  Ω2]
        [(omegas? Ω1)  Ω2]
        [(omegas? Ω2)  Ω1]
        [else  (define rect (indexed-rect-meet (λ ([A1 : Nonextremal-Real-Set]
                                                   [A2 : Nonextremal-Real-Set])
                                                 (define A (real-set-intersect A1 A2))
                                                 (if (empty-real-set? A) #f A))
                                               (Nonextremal-Omega-Set-rect Ω1)
                                               (Nonextremal-Omega-Set-rect Ω2)))
               (if rect (Nonextremal-Omega-Set rect) empty-omega-set)]))

(: omega-set-join (case-> (-> Omega-Set Nonempty-Omega-Set Nonempty-Omega-Set)
                          (-> Nonempty-Omega-Set Omega-Set Nonempty-Omega-Set)
                          (-> Omega-Set Omega-Set Omega-Set)))
(define (omega-set-join Ω1 Ω2)
  (cond [(omegas? Ω1)  Ω1]
        [(omegas? Ω2)  Ω2]
        [(empty-omega-set? Ω1)  Ω2]
        [(empty-omega-set? Ω2)  Ω1]
        [else  (define rect (indexed-rect-join (λ ([A1 : Nonextremal-Real-Set]
                                                   [A2 : Nonextremal-Real-Set])
                                                 (define A (real-set-union A1 A2))
                                                 (if (reals? A) #t A))
                                               (Nonextremal-Omega-Set-rect Ω1)
                                               (Nonextremal-Omega-Set-rect Ω2)))
               (if (true? rect) omegas (Nonextremal-Omega-Set rect))]))

(: omega-set-subseteq? (-> Omega-Set Omega-Set Boolean))
(define (omega-set-subseteq? Ω1 Ω2)
  (cond [(empty-omega-set? Ω1)  #t]
        [(empty-omega-set? Ω2)  #f]
        [(omegas? Ω2)  #t]
        [(omegas? Ω1)  #f]
        [else  (indexed-rect-subseteq? real-set-subseteq?
                                       (Nonextremal-Omega-Set-rect Ω1)
                                       (Nonextremal-Omega-Set-rect Ω2))]))

(: omega-set-proj (case-> (Empty-Omega-Set Tree-Index -> Empty-Real-Set)
                          (Full-Omega-Set Tree-Index -> Full-Real-Set)
                          (Nonempty-Omega-Set Tree-Index -> Nonempty-Real-Set)
                          (Omega-Set Tree-Index -> Real-Set)))
(define (omega-set-proj Ω j)
  (cond [(empty-omega-set? Ω)  empty-real-set]
        [(omegas? Ω)   reals]
        [else  (define B (indexed-rect-ref (Nonextremal-Omega-Set-rect Ω) j))
               (if (true? B) reals B)]))

(: omega-set-unproj (case-> (Empty-Omega-Set Tree-Index Real-Set -> Empty-Omega-Set)
                            (Omega-Set Tree-Index Empty-Real-Set -> Empty-Omega-Set)
                            (Full-Omega-Set Tree-Index Full-Real-Set -> Full-Omega-Set)
                            (Full-Omega-Set Tree-Index Nonempty-Real-Set -> Nonempty-Omega-Set)
                            (Nonempty-Omega-Set Tree-Index Full-Real-Set -> Nonempty-Omega-Set)
                            (Nonfull-Omega-Set Tree-Index Real-Set -> Nonfull-Omega-Set)
                            (Omega-Set Tree-Index Nonfull-Real-Set -> Nonfull-Omega-Set)
                            (Omega-Set Tree-Index Real-Set -> Omega-Set)))
(define (omega-set-unproj Ω j B)
  (cond [(empty-omega-set? Ω)  empty-omega-set]
        [(empty-real-set? B)   empty-omega-set]
        [(reals? B)  Ω]
        [else
         (define rect (indexed-rect-unproj (λ ([B1 : Nonextremal-Real-Set]
                                               [B2 : Nonextremal-Real-Set])
                                             (define B (real-set-intersect B1 B2))
                                             (if (empty-real-set? B) #f B))
                                           (if (omegas? Ω) #t (Nonextremal-Omega-Set-rect Ω))
                                           j
                                           B))
         (cond [(omegas? Ω)  (Nonextremal-Omega-Set (assert rect values))]
               [rect  (Nonextremal-Omega-Set rect)]
               [else  empty-omega-set])]))

(: omega-set-sample-point (-> Nonempty-Omega-Set Omega))
(define (omega-set-sample-point Ω)
  (Omega (indexed-rect-sample-point random
                                    real-set-sample-point
                                    (if (omegas? Ω) #t (Nonextremal-Omega-Set-rect Ω)))))

(: omega-set-measure (-> Omega-Set Flonum))
(define (omega-set-measure Ω)
  (cond [(empty-omega-set? Ω)  0.0]
        [else  (indexed-rect-measure real-set-measure
                                     (if (omegas? Ω) #t (Nonextremal-Omega-Set-rect Ω)))]))

(: omega-set->list (-> Omega-Set (Listof Nonempty-Real-Set)))
(define (omega-set->list Ω)
  (cond [(empty-omega-set? Ω)  empty]
        [else  (indexed-rect->list (if (omegas? Ω) #t (Nonextremal-Omega-Set-rect Ω)))]))

;; ===================================================================================================
;; Trace sets

(struct: Base-Trace-Set Base-Bot-Basic () #:transparent)
(define trace-set? Base-Trace-Set?)

(define-singleton-type Empty-Trace-Set Base-Trace-Set empty-trace-set)
(define-singleton-type Full-Trace-Set Base-Trace-Set traces)
(struct Nonextremal-Trace-Set Base-Trace-Set ([rect : (Indexed-Rect Nonextremal-Bool-Set)])
  #:transparent)

(define-type Nonfull-Trace-Set (U Empty-Trace-Set Nonextremal-Trace-Set))
(define-type Nonempty-Trace-Set (U Full-Trace-Set Nonextremal-Trace-Set))
(define-type Trace-Set (U Empty-Trace-Set Full-Trace-Set Nonextremal-Trace-Set))

(: trace-set-axis (-> Trace-Set Bool-Set))
(define (trace-set-axis T)
  (cond [(empty-trace-set? T)  empty-bool-set]
        [(traces? T)  bools]
        [else  (define B (indexed-rect-value (Nonextremal-Trace-Set-rect T)))
               (if (true? B) bools B)]))

(: trace-set-left (-> Trace-Set Trace-Set))
(define (trace-set-left T)
  (cond [(empty-trace-set? T)  empty-trace-set]
        [(traces? T)  traces]
        [else  (let ([t  (indexed-rect-left (Nonextremal-Trace-Set-rect T))])
                 (if (true? t) traces (Nonextremal-Trace-Set t)))]))

(: trace-set-right (-> Trace-Set Trace-Set))
(define (trace-set-right T)
  (cond [(empty-trace-set? T)  empty-trace-set]
        [(traces? T)  traces]
        [else  (let ([t  (indexed-rect-right (Nonextremal-Trace-Set-rect T))])
                 (if (true? t) traces (Nonextremal-Trace-Set t)))]))

(: trace-set (-> Bool-Set Trace-Set Trace-Set Trace-Set))
(define (trace-set B T1 T2)
  (cond [(or (empty-bool-set? B) (empty-trace-set? T1) (empty-trace-set? T2))  empty-trace-set]
        [(and (bools? B) (traces? T1) (traces? T2))  traces]
        [else  (Nonextremal-Trace-Set ((inst Indexed-Rect Nonextremal-Bool-Set)
                                       (if (bools? B) #t B)
                                       (if (traces? T1) #t (Nonextremal-Trace-Set-rect T1))
                                       (if (traces? T2) #t (Nonextremal-Trace-Set-rect T2))))]))

(: trace-set-projs (-> Trace-Set (Values Bool-Set Trace-Set Trace-Set)))
(define (trace-set-projs T)
  (cond [(empty-trace-set? T)  (values empty-bool-set empty-trace-set empty-trace-set)]
        [(traces? T)  (values bools traces traces)]
        [else  (match-define (Indexed-Rect B T1 T2) (Nonextremal-Trace-Set-rect T))
               (values (if (true? B) bools B)
                       (if (true? T1) traces (Nonextremal-Trace-Set T1))
                       (if (true? T2) traces (Nonextremal-Trace-Set T2)))]))

(: trace-set-unaxis (-> Trace-Set Bool-Set Trace-Set))
(define (trace-set-unaxis T B)
  (cond [(or (empty-trace-set? T) (empty-bool-set? B))  empty-trace-set]
        [else
         (define-values (B* T1 T2) (trace-set-projs T))
         (trace-set (bool-set-intersect B* B) T1 T2)]))

(: trace-set-unleft (-> Trace-Set Trace-Set Trace-Set))
(define (trace-set-unleft T T1)
  (cond [(or (empty-trace-set? T) (empty-trace-set? T1))  empty-trace-set]
        [else
         (define-values (B T1* T2) (trace-set-projs T))
         (trace-set B T1 #;(trace-set-intersect T1* T1) T2)]))

(: trace-set-unright (-> Trace-Set Trace-Set Trace-Set))
(define (trace-set-unright T T2)
  (cond [(or (empty-trace-set? T) (empty-trace-set? T2))  empty-trace-set]
        [else
         (define-values (B T1 T2*) (trace-set-projs T))
         (trace-set B T1 T2 #;(trace-set-intersect T2* T2))]))

(: trace-set-member? (-> Trace-Set Trace Boolean))
(define (trace-set-member? T t)
  (cond [(empty-trace-set? T)  #f]
        [(traces? T)  #t]
        [else  (indexed-rect-member? (λ ([B : Bool-Set] [b : (U Boolean Bottom)])
                                       (if (bottom? b) #f (bool-set-member? B b)))
                                     (Nonextremal-Trace-Set-rect T)
                                     (Trace-value t))]))

(: trace-set-intersect (case-> (-> Trace-Set Nonfull-Trace-Set Nonfull-Trace-Set)
                               (-> Nonfull-Trace-Set Trace-Set Nonfull-Trace-Set)
                               (-> Trace-Set Trace-Set Trace-Set)))
(define (trace-set-intersect T1 T2)
  (cond [(empty-trace-set? T1)  T1]
        [(empty-trace-set? T2)  T2]
        [(traces? T1)  T2]
        [(traces? T2)  T1]
        [else  (define rect (indexed-rect-meet (λ ([B1 : Nonextremal-Bool-Set]
                                                   [B2 : Nonextremal-Bool-Set])
                                                 (define B (bool-set-intersect B1 B2))
                                                 (if (empty-bool-set? B) #f B))
                                               (Nonextremal-Trace-Set-rect T1)
                                               (Nonextremal-Trace-Set-rect T2)))
               (if rect (Nonextremal-Trace-Set rect) empty-trace-set)]))

(: trace-set-join (case-> (-> Trace-Set Nonempty-Trace-Set Nonempty-Trace-Set)
                          (-> Nonempty-Trace-Set Trace-Set Nonempty-Trace-Set)
                          (-> Trace-Set Trace-Set Trace-Set)))
(define (trace-set-join T1 T2)
  (cond [(traces? T1)  T1]
        [(traces? T2)  T2]
        [(empty-trace-set? T1)  T2]
        [(empty-trace-set? T2)  T1]
        [else  (define rect (indexed-rect-join (λ ([B1 : Nonextremal-Bool-Set]
                                                   [B2 : Nonextremal-Bool-Set])
                                                 (define B (bool-set-union B1 B2))
                                                 (if (bools? B) #t B))
                                               (Nonextremal-Trace-Set-rect T1)
                                               (Nonextremal-Trace-Set-rect T2)))
               (if (true? rect) traces (Nonextremal-Trace-Set rect))]))

(: trace-set-subseteq? (-> Trace-Set Trace-Set Boolean))
(define (trace-set-subseteq? T1 T2)
  (cond [(empty-trace-set? T1)  #t]
        [(empty-trace-set? T2)  #f]
        [(traces? T2)  #t]
        [(traces? T1)  #f]
        [else  (indexed-rect-subseteq? bool-set-subseteq?
                                       (Nonextremal-Trace-Set-rect T1)
                                       (Nonextremal-Trace-Set-rect T2))]))

(: trace-set-proj (case-> (Empty-Trace-Set Tree-Index -> Empty-Bool-Set)
                          (Full-Trace-Set Tree-Index -> Full-Bool-Set)
                          (Nonempty-Trace-Set Tree-Index -> Nonempty-Bool-Set)
                          (Trace-Set Tree-Index -> Bool-Set)))
(define (trace-set-proj T j)
  (cond [(empty-trace-set? T)  empty-bool-set]
        [(traces? T)   bools]
        [else  (define B (indexed-rect-ref (Nonextremal-Trace-Set-rect T) j))
               (if (true? B) bools B)]))

(: trace-set-unproj (case-> (Empty-Trace-Set Tree-Index Bool-Set -> Empty-Trace-Set)
                            (Trace-Set Tree-Index Empty-Bool-Set -> Empty-Trace-Set)
                            (Full-Trace-Set Tree-Index Full-Bool-Set -> Full-Trace-Set)
                            (Full-Trace-Set Tree-Index Nonempty-Bool-Set -> Nonempty-Trace-Set)
                            (Nonempty-Trace-Set Tree-Index Full-Bool-Set -> Nonempty-Trace-Set)
                            (Nonfull-Trace-Set Tree-Index Bool-Set -> Nonfull-Trace-Set)
                            (Trace-Set Tree-Index Nonfull-Bool-Set -> Nonfull-Trace-Set)
                            (Trace-Set Tree-Index Bool-Set -> Trace-Set)))
(define (trace-set-unproj T j B)
  (cond [(empty-trace-set? T)  empty-trace-set]
        [(empty-bool-set? B)   empty-trace-set]
        [(bools? B)  T]
        [else
         (define rect (indexed-rect-unproj (λ ([B1 : Nonextremal-Bool-Set]
                                               [B2 : Nonextremal-Bool-Set])
                                             (define B (bool-set-intersect B1 B2))
                                             (if (empty-bool-set? B) #f B))
                                           (if (traces? T) #t (Nonextremal-Trace-Set-rect T))
                                           j
                                           B))
         (cond [(traces? T)  (Nonextremal-Trace-Set (assert rect values))]
               [rect  (Nonextremal-Trace-Set rect)]
               [else  empty-trace-set])]))

(: make-bottom-trace-value (-> Tree-Index Bottom))
(define (make-bottom-trace-value j)
  (bottom (delay (format "no branch decision at index ~a" j))))

(: make-bottom-trace-point (-> Tree-Index (Indexed-Point (U Boolean Bottom))))
(define (make-bottom-trace-point j)
  (Indexed-Point (delay (make-bottom-trace-value j))
                 (delay (make-bottom-trace-point (left j)))
                 (delay (make-bottom-trace-point (right j)))))

(define bottom-trace (Trace (make-bottom-trace-point j0)))

(: trace-set-infimum (-> Trace-Set Trace))
(define (trace-set-infimum T)
  (cond [(empty-trace-set? T)  bottom-trace]
        [(traces? T)  bottom-trace]
        [else
         (define t (Nonextremal-Trace-Set-rect T))
         (Trace
          (let loop ([t : (U #t (Indexed-Rect Nonextremal-Bool-Set))  t] [j : Tree-Index  j0])
            (cond [(true? t)  (make-bottom-trace-point j)]
                  [else
                   (match-define (Indexed-Rect v l r) t)
                   (Indexed-Point (delay (cond [(trues? v)  #t]
                                               [(falses? v)  #f]
                                               [else  (make-bottom-trace-value j)]))
                                  (delay (loop l (left j)))
                                  (delay (loop r (right j))))])))]))

(: bool-set-sample-point (-> Nonempty-Bool-Set Boolean))
(define (bool-set-sample-point B)
  (cond [(bools? B)  (< (random) 0.5)]
        [(trues? B)  #t]
        [(falses? B)  #f]))

(: trace-set-sample-point (-> Nonempty-Trace-Set Trace))
(define (trace-set-sample-point T)
  (define t (if (traces? T) #t (Nonextremal-Trace-Set-rect T)))
  (Trace
   (let loop ([t t])
     (cond [(true? t)  (loop (Indexed-Rect #t #t #t))]
           [else
            (match-define (Indexed-Rect v l r) t)
            (Indexed-Point (delay (cond [(trues? v)  #t]
                                        [(falses? v)  #f]
                                        [else  (< (random) 0.5)]))
                           (delay (loop l))
                           (delay (loop r)))]))))
