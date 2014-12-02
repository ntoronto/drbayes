#lang typed/racket/base

(require racket/match
         racket/list
         "../set.rkt"
         "../arrow.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt"
         "../flonum.rkt"
         "search-tree.rkt"
         "types.rkt"
         "parameters.rkt"
         "indexes.rkt"
         "refinement-search.rkt")

(provide (all-defined-out))

(: store-set-fill-branches (-> Store-Set Store-Set))
(define (store-set-fill-branches S)
  (cond [(empty-store-set? S)  S]
        [else
         (let loop ([S S])
           (cond [(stores? S)  S]
                 [else
                  (match-define (Plain-Store-Set X B L R) S)
                  (store-set X bools (loop L) (loop R))]))]))

(: store-set-list-self-intersections (-> (Listof Store-Set) (Listof Nonempty-Store-Set)))
(define (store-set-list-self-intersections Ss)
  (let ([Ss  (list->vector (map store-set-fill-branches Ss))])
    (define n (vector-length Ss))
    (for*/list : (Listof Nonempty-Store-Set) ([i  (in-range n)]
                                              [j  (in-range (+ i 1) n)]
                                              [U  (in-value (store-set-intersect
                                                             (vector-ref Ss i)
                                                             (vector-ref Ss j)))]
                                              #:unless (empty-store-set? U))
      U)))

(: store-set-list-random-outer-measure (-> (Listof Store-Set) Prob))
(define (store-set-list-random-outer-measure Ss)
  (cond [(empty? Ss)  prob-0]
        [else  (define p (prob+ (store-set-random-measure (first Ss))
                                (store-set-list-random-outer-measure (rest Ss))))
               (if (prob? p) p prob-1)]))

(: store-set-join* (-> (Listof Store-Set) Store-Set))
(define (store-set-join* Ss)
  (cond [(empty? Ss)  empty-store-set]
        [(empty? (rest Ss))  (first Ss)]
        [else
         (define m/2 (quotient (length Ss) 2))
         (store-set-join (store-set-join* (take Ss m/2))
                         (store-set-join* (drop Ss m/2)))]))

(: store-set-list-intersect (-> (Listof Store-Set) (Listof Store-Set) (Listof Nonempty-Store-Set)))
(define (store-set-list-intersect S0s S1s)
  ;(printf "length S0s = ~v  length S1s = ~v~n" (length S0s) (length S1s))
  (cond [(or (empty? S0s) (empty? S1s))  empty]
        [(and (empty? (rest S0s)) (empty? (rest S1s)))
         (define S (store-set-intersect (first S0s) (first S1s)))
         (if (empty-store-set? S) empty (list S))]
        [(empty-store-set? (store-set-intersect (store-set-join* S0s)
                                                (store-set-join* S1s)))
         empty]
        #;
        [(empty? (rest S0s))
         (define m/2 (quotient (length S1s) 2))
         (append (store-set-list-intersect S0s (take S1s m/2))
                 (store-set-list-intersect S0s (drop S1s m/2)))]
        #;
        [(empty? (rest S1s))
         (define m/2 (quotient (length S0s) 2))
         (append (store-set-list-intersect (take S0s m/2) S1s)
                 (store-set-list-intersect (drop S0s m/2) S1s))]
        [else
         (define m/2 (quotient (length S0s) 2))
         (define n/2 (quotient (length S1s) 2))
         (define S00s (take S0s m/2))
         (define S01s (drop S0s m/2))
         (define S10s (take S1s n/2))
         (define S11s (drop S1s n/2))
         (append (store-set-list-intersect S00s S10s)
                 (store-set-list-intersect S00s S11s)
                 (store-set-list-intersect S01s S10s)
                 (store-set-list-intersect S01s S11s))]))

(: store-set-list-random-measure (-> (Listof Store-Set) Prob))
; m(A ∪ B) = m(A) + m(B) - m(A ∩ B)
(define (store-set-list-random-measure Ss)
  (let loop ([Ss  (remove-duplicates (map  store-set-fill-branches Ss))])
    ;(printf "length Ss = ~v~n" (length Ss))
    (cond [(empty? Ss)  prob-0]
          [(empty? (rest Ss))  (store-set-random-measure (first Ss))]
          [else
           (define m/2 (quotient (length Ss) 2))
           (define S0s (take Ss m/2))
           (define S1s (drop Ss m/2))
           (define S2s (store-set-list-intersect S0s S1s))
           (define q (loop S2s))
           (define p0 (loop S0s))
           (define p1 (loop S1s))
           (let ([p0  (prob-min p0 p1)]
                 [p1  (prob-max p0 p1)])
             (let* ([p1  (prob- p1 q)]
                    [p1  (if (prob? p1) p1 prob-0)]
                    [p  (prob+ p0 p1)]
                    [p  (if (prob? p) p prob-1)])
               p))])))

(: store-set-list-random-measure/rndu (-> (Listof Store-Set) Prob))
(define (store-set-list-random-measure/rndu Ss)
  (store-set-list-random-outer-measure Ss))

(: drbayes-enumerate (-> meaning (Listof Nonempty-Store-Set)))
(define (drbayes-enumerate e)
  (define-values (h idxs)
    (match-let ([(meaning _ _ h k)  e])
      (values (run/pre* h) (k j0))))
  
  (define refine (make-preimage-refiner h universe))
  
  (define S (refine stores))
  (cond
    [(empty-store-set? S)  empty]
    [else
     (define t (build-search-tree refine S (annotate-indexes idxs)))
     
     (let loop : (Listof Nonempty-Store-Set) ([t : Store-Search-Tree  t])
       (cond [(search-node? t)
              (append (loop (maybe-force (search-node-fst t)))
                      (loop (maybe-force (search-node-snd t))))]
             [else
              (define S (search-leaf-value t))
              (cond [(empty-store-set? S)  empty]
                    [(prob-0? (store-set-random-measure S))  empty]
                    [else  (list S)])]))]))

(: drbayes-enumerate* (->* [meaning] [Natural] (Listof Nonempty-Store-Set)))
(define (drbayes-enumerate* e [n 100])
  (define s (drbayes-enumerate-relative-prob-min))
  (parameterize ([drbayes-refinement-axis-prob-min  prob-0]
                 [drbayes-refinement-prob-min  (drbayes-enumerate-prob-min)])
    (let loop ([i : Natural  1])
      (define m (drbayes-refinement-prob-min))
      (printf "~a: enumerating with min prob ~v~n" i (prob->flonum m))
      (define Ss (drbayes-enumerate e))
      (printf "enumeration length: ~v~n" (length Ss))
      (cond [(>= i n)  Ss]
            [else
             (define p (store-set-list-random-measure Ss))
             (printf "enumeration measure: ~v~n" p)
             (define new-m (prob* s (prob-max m (prob-min p (prob1- p)))))
             (cond [(prob>= new-m m)  Ss]
                   [else  (parameterize ([drbayes-refinement-prob-min  new-m])
                            (loop (+ i 1)))])]))))
