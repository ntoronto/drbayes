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
         "indexes.rkt"
         "split.rkt"
         "utils.rkt")

(provide (all-defined-out))

(define prob-0.8 (assert (flonum->prob 0.8) prob?))
(define prob-big (assert (flonum->prob 0.999) prob?))

(define empty-search-leaf (search-leaf empty-store-set prob-0))

(: prob-set-join* (-> (Listof+1 Nonempty-Prob-Interval) Nonempty-Prob-Set))
(define (prob-set-join* Is)
  (for/fold ([I : Nonempty-Prob-Set  (first Is)]) ([Ij  (in-list (rest Is))])
    (prob-set-join I Ij)))

;; ===================================================================================================

(: random-split (-> Refiner Nonempty-Store-Set Store-Index (U #f Interval-Splitter)
                    (Values Store-Set Store-Set)))
(define (random-split refine S j split)
  (define I (store-set-random-proj S j))
  (define Is
    (cond [(Plain-Prob-Interval-List? I)  (Plain-Prob-Interval-List-elements I)]
          [split  (split I)]
          [else   (interval-split I)]))
  (cond
    [(empty? Is)
     (values empty-store-set
             empty-store-set)]
    [(empty? (rest Is))
     (values empty-store-set
             (refine (store-set-random-unproj S j (first Is))))]
    [else
     (define n/2 (quotient (length Is) 2))
     (define I1 (prob-set-join* (assert (take Is n/2) pair?)))
     (define I2 (prob-set-join* (assert (drop Is n/2) pair?)))
     (values (refine (store-set-random-unproj S j I1))
             (refine (store-set-random-unproj S j I2)))]))

(: branch-split (-> Refiner Nonempty-Store-Set Store-Index (Values Store-Set Store-Set)))
(define (branch-split refine S j)
  (define B (store-set-branch-proj S j))
  (cond [(trues? B)   (values S empty-store-set)]
        [(falses? B)  (values empty-store-set S)]
        [else  (values (refine (store-set-branch-unproj S j trues))
                       (refine (store-set-branch-unproj S j falses)))]))

(: refine-ann-ifte*-indexes (-> Refiner Nonempty-Store-Set Ann-Indexes
                                (Values Store-Set Ann-Indexes)))
(define (refine-ann-ifte*-indexes refine S idxs)
  (let loop ([S S] [idxs idxs] [new-idxs : Ann-Indexes  empty])
    (match idxs
      [(list)  (values S (reverse new-idxs))]
      [(list (and idx (ann-ifte*-index j idxs1 idxs2)) idxs ...)
       (define S1 (refine (store-set-branch-unproj S j trues)))
       (define S2 (refine (store-set-branch-unproj S j falses)))
       (cond [(and (empty-store-set? S1) (empty-store-set? S2))  (values empty-store-set empty)]
             [(empty-store-set? S2)  (loop S1 (append (force idxs1) idxs) new-idxs)]
             [(empty-store-set? S1)  (loop S2 (append (force idxs2) idxs) new-idxs)]
             [else  (loop S idxs (cons idx new-idxs))])]
      [(list idx idxs ...)  (loop S idxs (cons idx new-idxs))])))

(: choose-index (-> Refiner Nonempty-Store-Set Ann-Indexes (Values Store-Set Ann-Indexes)))
(define (choose-index refine S idxs)
  (let-values ([(S idxs)  (refine-ann-ifte*-indexes refine S idxs)])
    (cond
      [(empty? idxs)  (values S idxs)]
      [else
       (cond [(ann-ifte*-index? (first idxs))  (values S idxs)]
             [else
              (define idx
                (argmax (λ ([idx : Ann-Index])
                          (cond [(ann-random-index? idx)
                                 (define j (ann-random-index-index idx))
                                 (prob->flonum (prob-set-measure (store-set-random-proj S j)))]
                                [else
                                 0.0]))
                        idxs))
              (values S (cons idx (remove idx idxs)))])])))

(: edges-shrink? (-> Refiner Nonempty-Store-Set Store-Index Boolean))
(define (edges-shrink? refine S j)
  (define target-p (drbayes-refinement-prob-min))
  (define p (store-set-random-measure S))
  (let* ([q  (prob/ target-p p)]
         [q  (if (prob? q) q prob-1)]
         [q  (prob-max q (drbayes-refinement-axis-prob-min))])
    (cond [(prob< q prob-0.5)
           (printf "1~n")
           (define X (store-set-random-proj S j))
           (cond
             [(Plain-Prob-Interval-List? X)  (printf "6~n") #t]
             [else
              (define-values (a b a? b?) (prob-interval-fields X))
              (define b1 (let ([b1  (prob-next a) #;(prob+ a q)]) (if (prob? b1) b1 prob-1)))
              (define a2 (let ([a2  (prob-prev b) #;(prob- b q)]) (if (prob? a2) a2 prob-0)))
              (define X1 (Plain-Prob-Interval a b1 a? #t))
              (define X2 (Plain-Prob-Interval a2 b #t b?))
              (define S1 (store-set-random-unproj S j X1))
              (define S2 (store-set-random-unproj S j X2))
              (cond [(empty-set? S1)  (printf "2~n") #t]
                    [(empty-set? S2)  (printf "3~n") #t]
                    [(not (equal? S1 (refine S1)))  (printf "4~n") #t]
                    [(not (equal? S2 (refine S2)))  (printf "5~n") #t]
                    [else  (printf "7~n") #f])])]
          [else
           #f])))

(: search-loop (-> Refiner Nonempty-Store-Set Ann-Indexes Store-Search-Tree))
(define (search-loop refine S idxs)
  (let loop ([S S] [idxs idxs] [back-idxs : Ann-Indexes  empty])
    (cond
      [(empty? idxs)
       (printf "search-loop: empty indexes 1~n")
       (search-leaf S (store-set-random-measure S))]
      [else
       (let-values ([(S idxs)  (choose-index refine S idxs)])
         (cond
           [(empty-store-set? S)
            (printf "search-loop: empty store set~n")
            empty-search-leaf]
           [(empty? idxs)
            (printf "search-loop: empty indexes 2~n")
            (search-leaf S (store-set-random-measure S))]
           [else
            (match idxs
              [(list (and idx (ann-random-index j split m)) idxs ...)
               (define X (store-set-random-proj S j))
               (define pj (prob-set-measure X))
               (cond
                 [(prob<= pj (drbayes-refinement-axis-prob-min))
                  (loop S idxs back-idxs)]
                 [else
                  (define-values (S1 S2) (random-split refine S j split))
                  (cond
                    [(and (empty-store-set? S1) (empty-store-set? S2))  empty-search-leaf]
                    [(empty-store-set? S1)  (loop S2 idxs (cons idx back-idxs))]
                    [(empty-store-set? S2)  (loop S1 idxs (cons idx back-idxs))]
                    [else
                     (define p1 (store-set-random-measure S1))
                     (define p2 (store-set-random-measure S2))
                     (define p (store-set-random-measure S))
                     (define dp
                       (let* ([new-p  (prob+/rndu p1 p2)]
                              [new-p  (if (prob? new-p) new-p p)]
                              [dp  (prob//rndu new-p p)]
                              [dp  (if (prob? dp) dp prob-1)])
                         dp))
                     
                     (define progress? (prob< dp prob-big))
                     (define should-split?
                       (or progress?
                           #t
                           #;(edges-shrink? refine (store-set-join S1 S2) j)))
                     (cond
                       [(and (= m 0) (not should-split?))
                        (loop (store-set-join S1 S2) idxs
                              (cons (ann-random-index j split 1) back-idxs))]
                       [else
                        (define new-m (if progress? 1 0))
                        (define idx (ann-random-index j split new-m))
                        (let ([idxs  (append idxs (reverse (cons idx back-idxs)))])
                          (search-node (delay (build-search-tree refine S1 idxs))
                                       (delay (build-search-tree refine S2 idxs))
                                       (prob-renormalize-first p1 p2)
                                       'random))])])])]
              [(list (and idx (ann-ifte*-index j idxs1 idxs2)) idxs ...)
               (define-values (S1 S2) (branch-split refine S j))
               (when (or (empty-store-set? S1) (empty-store-set? S2))
                 (when (empty-store-set? S1) (printf "S1 empty  "))
                 (when (empty-store-set? S2) (printf "S2 empty  "))
                 (newline))
               (cond [(and (empty-store-set? S1) (empty-store-set? S2))  empty-search-leaf]
                     [(empty-store-set? S1)  (loop S2 (append (force idxs2) idxs) back-idxs)]
                     [(empty-store-set? S2)  (loop S1 (append (force idxs1) idxs) back-idxs)]
                     [else
                      (define p1 (store-set-random-measure S1))
                      (define p2 (store-set-random-measure S2))
                      (let ([idxs1  (append (force idxs1) idxs (reverse back-idxs))]
                            [idxs2  (append (force idxs2) idxs (reverse back-idxs))])
                        (search-node (delay (build-search-tree refine S1 idxs1))
                                     (delay (build-search-tree refine S2 idxs2))
                                     (prob-renormalize-first (prob* p1 prob-0.5)
                                                             (prob* p2 prob-0.5))
                                     'branch))])])]))])))

;; ===================================================================================================

(: store-set-nonfull-axis-count (-> Nonempty-Store-Set Natural))
(define (store-set-nonfull-axis-count S)
  (cond [(stores? S)  0]
        [else  (match-define (Plain-Store-Set X B L R) S)
               (+ (if (probs? X) 0 1)
                  (store-set-nonfull-axis-count L)
                  (store-set-nonfull-axis-count R))]))

(: build-search-tree (-> Refiner Nonempty-Store-Set Ann-Indexes Store-Search-Tree))
(define (build-search-tree refine S idxs)
  ;(printf "idxs = ~v~n~n" idxs)
  (define p (store-set-random-measure S))
  (define num-axes (store-set-nonfull-axis-count S))
  (cond
    [(empty? idxs)
     (printf "build-search-tree: empty indexes~n")
     (search-leaf S p)]
    [(prob<= p (drbayes-refinement-prob-min))
     (printf "build-search-tree: low total probability~n")
     (search-leaf S p)]
    #;
    [(and (not (zero? num-axes))
          (<= (flexpt (prob->flonum p) (/ 1.0 (fl num-axes)))
              (prob->flonum (drbayes-refinement-axis-prob-min))))
     (printf "build-search-tree: low average axis probability~n")
     (search-leaf S p)]
    [else
     (search-loop refine S idxs)]))
