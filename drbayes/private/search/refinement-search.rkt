#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         math/flonum
         "../set.rkt"
         "../flonum.rkt"
         "../arrow.rkt"
         "../utils.rkt"
         "types.rkt"
         "parameters.rkt"
         "search-tree.rkt"
         "split.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define prob-branch-succ (assert (flonum->prob 0.8) prob?))
(define prob-big (assert (flonum->prob 0.999) prob?))

(define empty-search-leaf (search-fail empty-store-set))

(: prob-set-union* (-> (Listof+1 Nonempty-Prob-Interval) Nonempty-Prob-Set))
(define (prob-set-union* Is)
  (for/fold ([I : Nonempty-Prob-Set  (first Is)])
            ([Ij  (in-list (rest Is))])
    (prob-set-union I Ij)))

;; ===================================================================================================

(: random-split (-> Nonempty-Store-Set Store-Index (U Positive-Integer Interval-Splitter)
                    (Values Store-Set Store-Set)))
(define (random-split S j split)
  (define I (store-set-random-proj S j))
  (define Is
    (cond [(Prob-Interval-List? I)  (Prob-Interval-List-elements I)]
          [(integer? split)  (interval-split I)]
          [else  (split I)]))
  (cond
    [(empty? Is)
     (values empty-store-set
             empty-store-set)]
    [(empty? (rest Is))
     (values (store-set-random-unproj S j (first Is))
             empty-store-set)]
    [(empty? (rest (rest Is)))
     (values (store-set-random-unproj S j (first Is))
             (store-set-random-unproj S j (second Is)))]
    [else
     (define n/2 (quotient (length Is) 2))
     (define I1 (prob-set-union* (assert (take Is n/2) pair?)))
     (define I2 (prob-set-union* (assert (drop Is n/2) pair?)))
     (values (store-set-random-unproj S j I1)
             (store-set-random-unproj S j I2))]))

(: branch-split (-> Nonempty-Store-Set Store-Index (Values Store-Set Store-Set)))
(define (branch-split S j)
  (values (store-set-branch-unproj S j trues)
          (store-set-branch-unproj S j falses)))

;; ===================================================================================================

(: refine-ifte*-indexes (->* [Refiner Nonempty-Store-Set Indexes] [Natural]
                             (Values Store-Set Indexes)))
(define (refine-ifte*-indexes refine S idxs [depth 0])
  (let loop ([S S] [idxs idxs] [new-idxs : Indexes  empty] [depth depth])
    (cond
      [(= depth 0)  (values S (append (reverse new-idxs) idxs))]
      [else
       (match idxs
         [(list)  (values S (reverse new-idxs))]
         [(list (and idx (ifte*-index j idxs1 idxs2)) idxs ...)
          (define-values (S1 S1-exact?) (refine (store-set-branch-unproj S j trues)))
          (define-values (S2 S2-exact?) (refine (store-set-branch-unproj S j falses)))
          (cond [(and (empty-store-set? S1) (empty-store-set? S2))  (values empty-store-set empty)]
                [(empty-store-set? S2)  (loop S1 (append (force idxs1) idxs) new-idxs (- depth 1))]
                [(empty-store-set? S1)  (loop S2 (append (force idxs2) idxs) new-idxs (- depth 1))]
                [else  (loop S idxs (cons idx new-idxs) (- depth 1))])]
         [(list idx idxs ...)  (loop S idxs (cons idx new-idxs) depth)])])))

;; ===================================================================================================

(: choose-index (-> Nonempty-Store-Set Indexes Indexes))
(define (choose-index S idxs)
  (cond
    [#t  idxs]
    [(empty? idxs)  idxs]
    [(ifte*-index? (first idxs))  idxs]
    [else
     (define idx
       (argmin (λ ([idx : (U random-index ifte*-index)])
                 (cond [(random-index? idx)
                        (define j (random-index-index idx))
                        (prob->flonum (prob-set-measure (store-set-random-proj S j)))]
                       [else
                        0.0]))
               idxs))
     (cons idx (remove idx idxs))]))

(: search (-> Refiner Nonempty-Store-Set Indexes Store-Search-Tree))
(define (search refine S idxs)
  (let loop ([orig-S S] [S S] [idxs idxs] [back-idxs : Indexes  empty])
    ;(printf "idxs = ~v~n" idxs)
    (match (choose-index S idxs)
      [(list)
       (cond [(empty? back-idxs)
              ;(printf "search: empty indexes and back indexes~n")
              (search-succ S (store-set-random-measure S))]
             [(equal? orig-S S)
              ;(printf "search: empty indexes and same store~n")
              (search-succ S (store-set-random-measure S))]
             [else
              ;(printf "search: making progress shrinking store~n")
              ;; Making progress on shrinking the store (if not building the tree), so keep going
              (loop S S (reverse back-idxs) empty)])]
      [(list (and idx (random-index j orig-split)) idxs ...)
       (define split (if (not orig-split) (drbayes-refinement-max-splits) orig-split))
       (cond
         [(equal? split 0)  (loop orig-S S idxs back-idxs)]
         [else
          (let*-values
              (;; Split the store
               [(S1 S2)  (random-split S j split)]
               ;; Refine each half
               [(S1 S1-exact?)  (refine S1)]
               [(S2 S2-exact?)  (refine S2)]
               ;; Determine the new split field for this index
               [(split)  (if (integer? split) (- split 1) 0)]
               ;; If the new split field would be 0, don't use this index again
               [(back-idxs)  (cond [(zero? split)  back-idxs]
                                   [else  (cons (random-index j split) back-idxs)])])
            (cond
              [(and (empty-store-set? S1) (empty-store-set? S2))
               ;(printf "random: empty empty~n")
               empty-search-leaf]
              [(empty-store-set? S1)
               ;(printf "random: empty nonempty~n")
               (if S2-exact?
                   (search-succ S2 (store-set-random-measure S2))
                   (loop orig-S S2 idxs back-idxs))]
              [(empty-store-set? S2)
               ;(printf "random: nonempty empty~n")
               (if S1-exact?
                   (search-succ S1 (store-set-random-measure S1))
                   (loop orig-S S1 idxs back-idxs))]
              [else
               ;(printf "random: nonempty nonempty~n")
               (define p1 (store-set-random-measure S1))
               (define p2 (store-set-random-measure S2))
               (define new-idxs (delay (append idxs (reverse back-idxs))))
               (search-node (if S1-exact?
                                (delay (search-succ S1 p1))
                                (delay (build-search-tree refine S1 (force new-idxs))))
                            (if S2-exact?
                                (delay (search-succ S2 p2))
                                (delay (build-search-tree refine S2 (force new-idxs))))
                            (prob-normalize-first p1 p2)
                            'random)]))])]
      [(list (and idx (ifte*-index j idxs1 idxs2)) idxs ...)
       (define node
         (delay
           (let*-values ([(S1 S2)  (branch-split S j)]
                         [(S1 S1-exact?)  (refine S1)]
                         [(S2 S2-exact?)  (refine S2)])
             (cond
               [(and (empty-store-set? S1) (empty-store-set? S2))
                ;(printf "branch: empty empty~n")
                empty-search-leaf]
               [(empty-store-set? S1)
                ;(printf "branch: empty nonempty~n")
                (if S2-exact?
                    (search-succ S2 (store-set-random-measure S2))
                    (loop orig-S S2 (append (force idxs2) idxs) back-idxs))]
               [(empty-store-set? S2)
                ;(printf "branch: nonempty empty~n")
                (if S1-exact?
                    (search-succ S1 (store-set-random-measure S1))
                    (loop orig-S S1 (append (force idxs1) idxs) back-idxs))]
               [else
                ;(printf "branch: nonempty nonempty~n")
                (define p1 (store-set-random-measure S1))
                (define p2 (store-set-random-measure S2))
                (define new-idxs1 (delay (append (force idxs1) idxs (reverse back-idxs))))
                (define new-idxs2 (delay (append (force idxs2) idxs (reverse back-idxs))))
                (search-node (if S1-exact?
                                 (delay (search-succ S1 p1))
                                 (delay (build-search-tree refine S1 (force new-idxs1))))
                             (if S2-exact?
                                 (delay (search-succ S2 p2))
                                 (delay (build-search-tree refine S2 (force new-idxs2))))
                             (prob-normalize-first p1 p2)
                             'branch)]))))
       (if #t ;(drbayes-always-terminate?)
           (search-node node
                        (delay empty-search-leaf)
                        prob-branch-succ
                        'terminate)
           (force node))])))

;; ===================================================================================================

(: build-search-tree (-> Refiner Nonempty-Store-Set Indexes Store-Search-Tree))
(define (build-search-tree refine S idxs)
  ;(printf "idxs = ~v~n~n" idxs)
  (let-values ([(S idxs)  (refine-ifte*-indexes refine S idxs)])
    (define p (store-set-random-measure S))
    (cond
      [(empty-store-set? S)
       ;(printf "build-search-tree: empty store~n")
       empty-search-leaf]
      [(empty? idxs)
       ;(printf "build-search-tree: empty indexes~n")
       (search-succ S p)]
      [(prob<= p (drbayes-refinement-prob-min))
       ;(printf "build-search-tree: low total probability~n")
       (search-succ S p)]
      [else
       (search refine S idxs)])))
