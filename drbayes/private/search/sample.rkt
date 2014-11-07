#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         math/flonum
         math/distributions
         "../set.rkt"
         "../arrow.rkt"
         "search.rkt"
         "../utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================

(: interval-max-splits (Parameterof Natural))
(define interval-max-splits (make-parameter 5))

(: interval-min-length (Parameterof Nonnegative-Flonum))
(define interval-min-length (make-parameter 1e-14))

(define-type Ann-Index (U ann-random-index ann-ifte*-index))
(define-type Ann-Indexes (Listof Ann-Index))
(struct: ann-random-index random-index ([num-splits : Natural] [min-length : Flonum])
  #:transparent)
(struct: ann-ifte*-index ([index : Tree-Index]
                          [true : (Promise Ann-Indexes)]
                          [false : (Promise Ann-Indexes)])
  #:transparent)

(: annotate-indexes (Indexes -> Ann-Indexes))
(define (annotate-indexes idxs)
  (define m (interval-max-splits))
  (define l (interval-min-length))
  (let loop ([idxs idxs])
    (if (empty? idxs)
        idxs
        (let ([idx  (first idxs)]
              [idxs  (rest idxs)])
          (cond [(random-index? idx)
                 (match-define (random-index i split) idx)
                 (cons (ann-random-index i split m l) (loop idxs))]
                [else
                 (match-define (ifte*-index j t f) idx)
                 (cons (ann-ifte*-index j (delay (loop (force t))) (delay (loop (force f))))
                       (loop idxs))])))))

(: interval-split (Nonempty-Interval Flonum -> (Values (Listof Nonempty-Interval)
                                                       (Listof Flonum))))
(define (interval-split I p)
  (define-values (a b a? b?) (interval-fields I))
  (define c (fl* p (fl+ a b)))
  (define m1 (fl- c a))
  (define m2 (fl- b c))
  (if (and (positive? m1) (positive? m2))
      (values (list (Nonextremal-Interval a c a? #t)
                    (Nonextremal-Interval c b #f b?))
              (list m1 m2))
      (values (list I)
              (list 1.0))))

;; ===================================================================================================

(define-type Store (Pair Omega Trace))

(define-type Nonempty-Store-Rect (Pair Nonempty-Omega-Set Nonempty-Trace-Set))
(define-type Store-Rect (U Empty-Set Nonempty-Store-Rect))
(define-type Omega-Search-Tree (Search-Tree Store-Rect))

(struct: store-rect-sample ([rect : Nonempty-Store-Rect]
                            [measure : Flonum]
                            [prob : Flonum])
  #:transparent)

(struct: store-sample ([s : Store] [prob : Flonum])
  #:transparent)

;; ===================================================================================================
;; Preimage refinement

(define-type Refiner (Nonempty-Store-Rect -> Store-Rect))

(: store-rect->set (Nonempty-Store-Rect -> Nonempty-Set))
(define (store-rect->set S)
  (match-define (cons R T) S)
  (set-pair R T))

(: set->store-rect (Set -> Store-Rect))
(define (set->store-rect S)
  (cond [(empty-set? S)  empty-set]
        [(not (pair-set? S))
         (raise-argument-error 'set->store-rect "Pair-Set" S)]
        [else
         (define-values (R T) (set-projs S))
         (cond [(or (empty-set? R) (empty-set? T))  empty-set]
               [(not (omega-set? R))
                (raise-argument-error 'set->store-rect "Omega-Set" R)]
               [(not (trace-set? T))
                (raise-argument-error 'set->store-rect "Trace-Set" T)]
               [else
                (cons R T)])]))

(: preimage-refiner (Pre-Arrow Nonempty-Set -> Refiner))
(define (preimage-refiner h B)
  (: refine Refiner)
  (define (refine S)
    (define A (ap/pre (h (set-pair (store-rect->set S) nulls)) B))
    (define-values (S* N) (set-projs A))
    (cond [(not (or (empty-set? N) (nulls? N)))
           (raise-result-error 'preimage-refiner "(U Empty-Set Full-Null-Set)" N)]
          [else
           (set->store-rect S*)]))
  refine)

;; ===================================================================================================
;; Refinement sampling

(: refinement-sample* (Nonempty-Store-Rect Indexes Refiner Natural -> (Listof store-rect-sample)))
(define (refinement-sample* S idxs refine n)
  (define t (build-search-tree S (annotate-indexes idxs) refine))
  (define-values (ts ps _t _q) (sample-search-tree* t n))
  (let loop ([ts ts] [ps ps])
    (cond [(or (empty? ts) (empty? ps))  empty]
          [else
           (match-define (search-leaf S m) (first ts))
           (define p (first ps))
           (cond [(empty-set? S)  (loop (rest ts) (rest ps))]
                 [else
                  (cons (store-rect-sample S m p) (loop (rest ts) (rest ps)))])])))

(: expand-ann-indexes (Nonempty-Store-Rect Ann-Indexes Refiner -> Ann-Indexes))
(define (expand-ann-indexes S idxs refine)
  (match-define (cons R T) S)
  (let loop ([idxs idxs])
    (if (empty? idxs)
        idxs
        (let ([idx  (first idxs)]
              [idxs  (rest idxs)])
          (cond [(ann-ifte*-index? idx)
                 (match-define (ann-ifte*-index j t-idxs f-idxs) idx)
                 (define b (trace-set-proj T j))
                 (cond [(eq? b trues)   (append (force t-idxs) (loop idxs))]
                       [(eq? b falses)  (append (force f-idxs) (loop idxs))]
                       [else  (cons idx (loop idxs))])]
                [else
                 (cons idx (loop idxs))])))))

(: ann-index< (Nonempty-Store-Rect Refiner -> (Ann-Index Ann-Index -> Boolean)))
(define ((ann-index< S refine) idx0 idx1)
  (match-define (cons R T) S)
  (cond [(ann-ifte*-index? idx0)  #f]
        [(ann-ifte*-index? idx1)  #t]
        [else
         (match-define (ann-random-index j0 split0 m0 l0) idx0)
         (match-define (ann-random-index j1 split1 m1 l1) idx1)
         ;((interval*-measure (omega-set-proj R j0)) . > . (interval*-measure (omega-set-proj R j1)))
         (m0 . > . m1)
         ]))

(: choose-ann-index (Nonempty-Store-Rect Ann-Indexes Refiner -> (Values Ann-Index Ann-Indexes)))
(define (choose-ann-index S idxs refine)
  (let (#;[idxs  (sort idxs (ann-index< S refine))])
    (values (first idxs) (rest idxs))))

(: build-search-tree (Nonempty-Store-Rect Ann-Indexes Refiner -> Omega-Search-Tree))
(define (build-search-tree S idxs refine)
  (cond
    [(empty-set? S)  (search-leaf empty-set 0.0)]
    [else
     (let ([idxs  (expand-ann-indexes S idxs refine)])
       (cond [(empty? idxs)  (search-leaf S (omega-set-measure (car S)))]
             [else
              (let-values ([(idx idxs)  (choose-ann-index S idxs refine)])
                (if (ann-ifte*-index? idx)
                    (build-search-tree/if S idx idxs refine)
                    (build-search-tree/ivl S idx idxs refine)))]))]))

(: build-search-tree/if (Nonempty-Store-Rect ann-ifte*-index Ann-Indexes Refiner
                                             -> Omega-Search-Tree))
(define (build-search-tree/if S idx idxs refine)
  (match-define (cons R T) S)
  (match-define (ann-ifte*-index j t-idxs f-idxs) idx)
  
  (: make-node (Nonempty-Bool-Set (Promise Ann-Indexes) -> Omega-Search-Tree))
  (define (make-node B b-idxs)
    (let ([T  (trace-set-unproj T j B)])
      (cond [(empty-trace-set? T)  (search-leaf empty-set 0.0)]
            [else  (let ([S  (refine (cons R T))])
                     (cond [(empty-set? S)  (search-leaf empty-set 0.0)]
                           [else  (build-search-tree S (append (force b-idxs) idxs) refine)]))])))
  
  (define B (trace-set-proj T j))
  (cond [(eq? B trues)   (make-node trues t-idxs)]
        [(eq? B falses)  (make-node falses f-idxs)]
        [(drbayes-always-terminate?)
         (search-node (list (delay (make-node trues t-idxs))
                            (delay (make-node falses f-idxs))
                            (search-leaf empty-set 0.0))
                      (list 0.4 0.4 0.2)
                      'probabilistic
                      'branches)]
        [else
         (search-node (list (delay (make-node trues t-idxs))
                            (delay (make-node falses f-idxs)))
                      (list 0.5 0.5)
                      'probabilistic
                      'branches)]))

(: proportional-split (Nonempty-Store-Rect Refiner Tree-Index Nonempty-Interval
                                           -> (Values (Listof Nonempty-Interval) (Listof Flonum))))
(define (proportional-split S refine i I)
  (match-define (cons R T) S)
  (define-values (Is ls) (interval-split I 0.5))
  (match Is
    [(list I0 I1)
     (define R0 (omega-set-unproj R i I0))
     (define R1 (omega-set-unproj R i I1))
     (define S0 (if (empty-omega-set? R0) empty-set (refine (cons R0 T))))
     (define S1 (if (empty-omega-set? R1) empty-set (refine (cons R1 T))))
     (define m0 (if (empty-set? S0) 0.0 (omega-set-measure (car S0))))
     (define m1 (if (empty-set? S1) 0.0 (omega-set-measure (car S1))))
     (cond [(and (positive? m0) (positive? m1))
            ;(values Is (list m0 m1))
            (values Is (map + (normalize-probs ls) (normalize-probs (list m0 m1))))
            ;(values Is ls)
            ]
           [(positive? m0)  (values (list I0) (list 1.0))]
           [(positive? m1)  (values (list I1) (list 1.0))]
           [else  (values Is ls)])]
    [_  (values Is ls)]))

(: build-search-tree/ivl (Nonempty-Store-Rect ann-random-index Ann-Indexes Refiner
                                              -> Omega-Search-Tree))
(define (build-search-tree/ivl S idx idxs refine)
  (match-define (ann-random-index j split m min-length) idx)
  (cond
    [(zero? m)  (build-search-tree S idxs refine)]
    [else
     (match-define (cons R T) S)
     (define I (omega-set-proj R j))
     (unless (real-set-subseteq? I unit-interval)
       (error 'build-search-tree/ivl "internal error: omega projection is ~a" I))
     (define-values (Is ls)
       (cond [(interval-list? I)  (define Is (interval-list-elements I))
                                  (values Is (map interval-measure Is))]
             [((interval-measure I) . < . min-length)  (values (list I) (list 1.0))]
             [split  (split I)]
             ;[else   (interval-split I 0.5)]
             [else   (proportional-split S refine j I)]
             ))
     (cond
       [(or (empty? Is) (empty? ls))
        (build-search-tree S idxs refine)]
       [(or (empty? (rest Is)) (empty? (rest ls)))
        (let* ([R  (omega-set-unproj R j (first Is))]
               [S  (if (empty-omega-set? R) empty-set (refine (cons R T)))])
          (cond [(empty-set? S)  (search-leaf empty-set 0.0)]
                [else  (define idx (ann-random-index j split (- m 1) min-length))
                       (build-search-tree S (cons idx idxs) refine)]))]
       [else
        (: make-node (Nonempty-Interval -> (Promise Omega-Search-Tree)))
        (define (make-node I)
          (delay
            (let* ([R  (omega-set-unproj R j I)]
                   [S  (if (empty-omega-set? R) empty-set (refine (cons R T)))])
              (cond [(empty-set? S)  (search-leaf empty-set 0.0)]
                    [else  (define idx (ann-random-index j split (- m 1) min-length))
                           (build-search-tree S (cons idx idxs) refine)]))))
        
        (search-node (map/+2 make-node Is)
                     (normalize-probs/+2 ls)
                     'probabilistic
                     'splits)])]))

;; ===================================================================================================

(: refinement-sample (Store-Rect Flonum Flonum Indexes Refiner -> (U #f store-rect-sample)))
(define (refinement-sample S m p idxs refine)
  (cond [(empty-set? S)  #f]
        [(empty? idxs)  (store-rect-sample S m p)]
        [else  (let ([idx  (first idxs)]
                     [idxs  (rest idxs)])
                 (if (ifte*-index? idx)
                     (refinement-sample/if S m p idx idxs refine)
                     (refinement-sample/ivl S m p idx idxs refine)))]))

(: refinement-sample/if (Nonempty-Store-Rect Flonum Flonum ifte*-index Indexes Refiner
                                             -> (U #f store-rect-sample)))
(define (refinement-sample/if S m p idx idxs refine)
  (match-define (cons R T) S)
  (match-define (ifte*-index j t-idxs f-idxs) idx)
  (define B (trace-set-proj T j))
  (cond [(eq? B trues)   (refinement-sample S m p (append (force t-idxs) idxs) refine)]
        [(eq? B falses)  (refinement-sample S m p (append (force f-idxs) idxs) refine)]
        [else  (define-values (B new-idxs q)
                 (cond [((random) . < . 0.5)  (values trues (force t-idxs) 0.5)]
                       [else                  (values falses (force f-idxs) 0.5)]))
               (let* ([T  (trace-set-unproj T j B)]
                      [S  (if (empty-trace-set? T) empty-set (refine (cons R T)))])
                 (refinement-sample S m (* p q) (append new-idxs idxs) refine))]))

(: refinement-sample/ivl (Nonempty-Store-Rect Flonum Flonum random-index Indexes Refiner
                                              -> (U #f store-rect-sample)))
(define (refinement-sample/ivl S m p idx idxs refine)
  (match-define (cons R T) S)
  (match-define (random-index j split) idx)
  (define I (omega-set-proj R j))
  (define x (if (reals? I) +nan.0 (real-set-sample-point I)))
  (define J (Nonextremal-Interval x x #t #t))
  (define q (real-set-measure I))
  (let* ([R  (omega-set-unproj R j J)]
         [S  (if (empty-omega-set? R) empty-set (refine (cons R T)))])
    (refinement-sample S (* m q) p idxs refine)))

;; ===================================================================================================

(: refinement-sample-point (Nonempty-Store-Rect Indexes Refiner -> (U #f store-sample)))
(define (refinement-sample-point S idxs refine)
  (match (refinement-sample S 1.0 1.0 idxs refine)
    [(store-rect-sample S m p)
     (match-define (cons R T) S)
     (define r (omega-set-sample-point R))
     (define t (trace-set-infimum T))
     (store-sample (cons r t) (/ m p))]
    [_  #f]))

;; ===================================================================================================
;; Front end to sampler

(: drbayes-sample (meaning Natural -> (Values (Listof Value) (Listof Flonum))))
(define (drbayes-sample e n)
  (define-values (f h idxs)
    (match-let ([(meaning _ f h k)  e])
      (values (run/bot* f) (run/pre* h) (k j0))))
  
  (define refine (preimage-refiner h universe))
  
  (define S
    (let ([S  (refine (cons omegas traces))])
      (if (empty-set? S)
          (error 'drbayes-sample "cannot sample from the empty set")
          S)))
  
  (define t (build-search-tree S (annotate-indexes idxs) refine))
  
  (let: loop ([i : Natural  0]
              [bs : (Listof Value)   empty]
              [ws : (Listof Flonum)  empty]
              [t : Omega-Search-Tree  t]
              [q : Flonum  1.0])
    (cond
      [(and (i . < . n) (q . > . 0.0))
       (let ([i  (+ i 1)])
         (when (= 0 (remainder i 100))
           (printf "i = ~v~n" i)
           (flush-output))
         (let-values ([(leaf-ts leaf-ps t q)  (sample-search-tree t q)])
           (let: inner-loop ([leaf-ts leaf-ts]
                             [leaf-ps leaf-ps]
                             [bs : (Listof Value)   bs]
                             [ws : (Listof Flonum)  ws])
             (cond
               [(or (empty? leaf-ts) (empty? leaf-ps))
                (loop i bs ws t q)]
               [else
                (define leaf-t (first leaf-ts))
                (define leaf-p (first leaf-ps))
                (match-define (search-leaf S m) leaf-t)
                (cond
                  [(or (empty-set? S) (m . <= . 0.0))
                   ;(printf "sample-search-tree returned failure~n")
                   (inner-loop (rest leaf-ts) (rest leaf-ps) bs ws)]
                  [else
                   (define pt (refinement-sample-point S idxs refine))
                   ;(match-define (cons R T) S)
                   ;(define r (omega-set-sample-point R))
                   ;(define t (trace-set-infimum T))
                   ;(define pt (store-sample (cons r t) m))
                   (match pt
                     [(store-sample s m)
                      (define b (f (cons s null)))
                      (cond [(bottom? b)
                             ;(printf "b = bottom: ~a~n" (force (bottom-message b)))
                             (inner-loop (rest leaf-ts) (rest leaf-ps) bs ws)]
                            [else
                             ;(printf "success!~n")
                             (inner-loop (rest leaf-ts) (rest leaf-ps)
                                         (cons b bs) (cons (/ m leaf-p) ws))])]
                     [_
                      ;(printf "refinement-sample-point returned #f~n")
                      (inner-loop (rest leaf-ts) (rest leaf-ps) bs ws)])])]))))]
      [else
       (when (q . <= . 0.0)
         (error 'drbayes-sample "cannot sample from a zero-probability set"))
       (values bs ws)])))
