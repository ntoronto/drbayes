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
(struct: ann-ifte*-index ([index : Store-Index]
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

(: interval-split (-> Nonempty-Real-Interval
                      Flonum
                      (Values (Listof Nonempty-Real-Interval)
                              (Listof Flonum))))
(define (interval-split I p)
  (define-values (a b a? b?) (real-interval-fields I))
  (define c (fl* p (fl+ a b)))
  (define m1 (fl- c a))
  (define m2 (fl- b c))
  (if (and (positive? m1) (positive? m2))
      (values (list (Plain-Real-Interval a c a? #t)
                    (Plain-Real-Interval c b #f b?))
              (list m1 m2))
      (values (list I)
              (list 1.0))))

;; ===================================================================================================

(define-type Omega-Search-Tree (Search-Tree Store-Set))

(struct: store-rect-sample ([rect : Nonempty-Store-Set]
                            [measure : Flonum]
                            [prob : Flonum])
  #:transparent)

(struct: store-sample ([s : Store] [prob : Flonum])
  #:transparent)

;; ===================================================================================================
;; Preimage refinement

(define-type Refiner (Store-Set -> Store-Set))

(: preimage-refiner (Pre-Arrow Nonempty-Set -> Refiner))
(define ((preimage-refiner h B) S)
  (cond [(empty-store-set? S)  empty-store-set]
        [else
         (let-values ([(S N)  (set-projs (preimage/pre (run/pre h (set-pair S nulls)) B))])
           (cond [(not (or (empty-set? N) (nulls? N)))
                  (raise-result-error 'preimage-refiner "(U Empty-Set Full-Null-Set)" N)]
                 [else
                  (set-take-stores S)]))]))

;; ===================================================================================================
;; Refinement sampling

(: refinement-sample* (Nonempty-Store-Set Indexes Refiner Natural -> (Listof store-rect-sample)))
(define (refinement-sample* S idxs refine n)
  (define t (build-search-tree S (annotate-indexes idxs) refine))
  (define-values (ts ps _t _q) (sample-search-tree* t n))
  (let loop ([ts ts] [ps ps])
    (cond [(or (empty? ts) (empty? ps))  empty]
          [else
           (match-define (search-leaf S m) (first ts))
           (define p (first ps))
           (cond [(empty-store-set? S)  (loop (rest ts) (rest ps))]
                 [else
                  (cons (store-rect-sample S m p) (loop (rest ts) (rest ps)))])])))

(: expand-ann-indexes (Nonempty-Store-Set Ann-Indexes Refiner -> Ann-Indexes))
(define (expand-ann-indexes S idxs refine)
  (let loop ([idxs idxs])
    (if (empty? idxs)
        idxs
        (let ([idx  (first idxs)]
              [idxs  (rest idxs)])
          (cond [(ann-ifte*-index? idx)
                 (match-define (ann-ifte*-index j t-idxs f-idxs) idx)
                 (define b (store-set-branch-proj S j))
                 (cond [(eq? b trues)   (append (force t-idxs) (loop idxs))]
                       [(eq? b falses)  (append (force f-idxs) (loop idxs))]
                       [else  (cons idx (loop idxs))])]
                [else
                 (cons idx (loop idxs))])))))

(: ann-index< (Nonempty-Store-Set Refiner -> (Ann-Index Ann-Index -> Boolean)))
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

(: choose-ann-index (Nonempty-Store-Set Ann-Indexes Refiner -> (Values Ann-Index Ann-Indexes)))
(define (choose-ann-index S idxs refine)
  (let (#;[idxs  (sort idxs (ann-index< S refine))])
    (values (first idxs) (rest idxs))))

(: build-search-tree (Nonempty-Store-Set Ann-Indexes Refiner -> Omega-Search-Tree))
(define (build-search-tree S idxs refine)
  (cond
    [(empty-store-set? S)  (search-leaf empty-store-set 0.0)]
    [else
     (let ([idxs  (expand-ann-indexes S idxs refine)])
       (cond [(empty? idxs)  (search-leaf S (store-set-random-measure S))]
             [else
              (let-values ([(idx idxs)  (choose-ann-index S idxs refine)])
                (if (ann-ifte*-index? idx)
                    (build-search-tree/if S idx idxs refine)
                    (build-search-tree/ivl S idx idxs refine)))]))]))

(: build-search-tree/if (Nonempty-Store-Set ann-ifte*-index Ann-Indexes Refiner
                                            -> Omega-Search-Tree))
(define (build-search-tree/if S idx idxs refine)
  (match-define (ann-ifte*-index j t-idxs f-idxs) idx)
  
  (: make-node (Nonempty-Bool-Set (Promise Ann-Indexes) -> Omega-Search-Tree))
  (define (make-node B b-idxs)
    (let ([S  (refine (store-set-branch-unproj S j B))])
      (cond [(empty-store-set? S)  (search-leaf empty-store-set 0.0)]
            [else  (build-search-tree S (append (force b-idxs) idxs) refine)])))
  
  (define B (store-set-branch-proj S j))
  (cond [(eq? B trues)   (make-node trues t-idxs)]
        [(eq? B falses)  (make-node falses f-idxs)]
        [(drbayes-always-terminate?)
         (search-node (list (delay (make-node trues t-idxs))
                            (delay (make-node falses f-idxs))
                            (search-leaf empty-store-set 0.0))
                      (list 0.4 0.4 0.2)
                      'probabilistic
                      'branches)]
        [else
         (search-node (list (delay (make-node trues t-idxs))
                            (delay (make-node falses f-idxs)))
                      (list 0.5 0.5)
                      'probabilistic
                      'branches)]))

(: proportional-split (-> Nonempty-Store-Set
                          Refiner
                          Store-Index
                          Nonempty-Real-Interval
                          (Values (Listof Nonempty-Real-Interval)
                                  (Listof Flonum))))
(define (proportional-split S refine i I)
  (define-values (Is ls) (interval-split I 0.5))
  (match Is
    [(list I0 I1)
     (define S0 (refine (store-set-random-unproj S i I0)))
     (define S1 (refine (store-set-random-unproj S i I1)))
     (define m0 (store-set-random-measure S0))
     (define m1 (store-set-random-measure S1))
     (cond [(and (positive? m0) (positive? m1))
            ;(values Is (list m0 m1))
            (values Is (map + (normalize-probs ls) (normalize-probs (list m0 m1))))
            ;(values Is ls)
            ]
           [(positive? m0)  (values (list I0) (list 1.0))]
           [(positive? m1)  (values (list I1) (list 1.0))]
           [else  (values Is ls)])]
    [_  (values Is ls)]))

(: build-search-tree/ivl (Nonempty-Store-Set ann-random-index Ann-Indexes Refiner
                                             -> Omega-Search-Tree))
(define (build-search-tree/ivl S idx idxs refine)
  (match-define (ann-random-index j split m min-length) idx)
  (cond
    [(zero? m)  (build-search-tree S idxs refine)]
    [else
     (define I (store-set-random-proj S j))
     (unless (real-set-subseteq? I unit-interval)
       (error 'build-search-tree/ivl "internal error: omega projection is ~a" I))
     (define-values (Is ls)
       (cond [(Plain-Real-Interval-List? I)
              (define Is (Plain-Real-Interval-List-elements I))
              (values Is (map real-interval-measure Is))]
             [(< (real-interval-measure I) min-length)  (values (list I) (list 1.0))]
             [split  (split I)]
             ;[else   (interval-split I 0.5)]
             [else   (proportional-split S refine j I)]
             ))
     (cond
       [(or (empty? Is) (empty? ls))
        (build-search-tree S idxs refine)]
       [(or (empty? (rest Is)) (empty? (rest ls)))
        (let ([S  (refine (store-set-random-unproj S j (first Is)))])
          (cond [(empty-store-set? S)  (search-leaf empty-store-set 0.0)]
                [else  (define idx (ann-random-index j split (- m 1) min-length))
                       (build-search-tree S (cons idx idxs) refine)]))]
       [else
        (: make-node (Nonempty-Real-Interval -> (Promise Omega-Search-Tree)))
        (define (make-node I)
          (delay
            (let ([S  (refine (store-set-random-unproj S j I))])
              (cond [(empty-store-set? S)  (search-leaf empty-store-set 0.0)]
                    [else  (define idx (ann-random-index j split (- m 1) min-length))
                           (build-search-tree S (cons idx idxs) refine)]))))
        
        (search-node (map/+2 make-node Is)
                     (normalize-probs/+2 ls)
                     'probabilistic
                     'splits)])]))

;; ===================================================================================================

(: refinement-sample (Store-Set Flonum Flonum Indexes Refiner -> (U #f store-rect-sample)))
(define (refinement-sample S m p idxs refine)
  (cond [(empty-store-set? S)  #f]
        [(empty? idxs)  (store-rect-sample S m p)]
        [else  (let ([idx  (first idxs)]
                     [idxs  (rest idxs)])
                 (if (ifte*-index? idx)
                     (refinement-sample/if S m p idx idxs refine)
                     (refinement-sample/ivl S m p idx idxs refine)))]))

(: refinement-sample/if (Nonempty-Store-Set Flonum Flonum ifte*-index Indexes Refiner
                                            -> (U #f store-rect-sample)))
(define (refinement-sample/if S m p idx idxs refine)
  (match-define (ifte*-index j t-idxs f-idxs) idx)
  (define B (store-set-branch-proj S j))
  (cond [(eq? B trues)   (refinement-sample S m p (append (force t-idxs) idxs) refine)]
        [(eq? B falses)  (refinement-sample S m p (append (force f-idxs) idxs) refine)]
        [else  (define-values (B new-idxs q)
                 (cond [((random) . < . 0.5)  (values trues (force t-idxs) 0.5)]
                       [else                  (values falses (force f-idxs) 0.5)]))
               (let ([S  (refine (store-set-branch-unproj S j B))])
                 (refinement-sample S m (* p q) (append new-idxs idxs) refine))]))

(: refinement-sample/ivl (Nonempty-Store-Set Flonum Flonum random-index Indexes Refiner
                                             -> (U #f store-rect-sample)))
(define (refinement-sample/ivl S m p idx idxs refine)
  (match-define (random-index j split) idx)
  (define I (store-set-random-proj S j))
  (define x (if (reals? I) +nan.0 (real-set-sample-point I)))
  (define J (Plain-Real-Interval x x #t #t))
  (define q (real-set-measure I))
  (let ([S  (refine (store-set-random-unproj S j J))])
    (refinement-sample S (* m q) p idxs refine)))

;; ===================================================================================================

(: refinement-sample-point (Nonempty-Store-Set Indexes Refiner -> (U #f store-sample)))
(define (refinement-sample-point S idxs refine)
  (match (refinement-sample S 1.0 1.0 idxs refine)
    [(store-rect-sample S m p)
     (store-sample (store-set-realize S) (/ m p))]
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
    (let ([S  (refine stores)])
      (if (empty-store-set? S)
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
           ;(printf "tree size: ~v~n~n" (search-tree-size t))
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
                ;(printf "S = ~v~n~n" S)
                (cond
                  [(empty-store-set? S)
                   ;(printf "sample-search-tree returned failure~n")
                   (inner-loop (rest leaf-ts) (rest leaf-ps) bs ws)]
                  [else
                   (define pt (refinement-sample-point S idxs refine))
                   ;(define pt (store-sample (store-set-realize S) m))
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
       (cond [(and (search-leaf? t) (empty-store-set? (search-leaf-value t)))
              (error 'drbayes-sample "cannot sample from the empty set")]
             [(q . <= . 0.0)
              (error 'drbayes-sample "cannot sample from a zero-probability set")])
       (values bs ws)])))
