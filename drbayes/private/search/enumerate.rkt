#lang typed/racket/base

(require racket/match
         (only-in racket/list
                  first second rest last empty? empty take drop append*
                  remove-duplicates)
         (prefix-in r. racket/set)
         math/statistics
         "../set.rkt"
         "../arrow.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt"
         "../flonum.rkt"
         "../language.rkt"
         "search-tree.rkt"
         "types.rkt"
         "parameters.rkt"
         "refinement-search.rkt"
         "utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Store set utils

(: store-set-fill-branches (case-> (-> Nonempty-Store-Set Nonempty-Store-Set)
                                   (-> Store-Set Store-Set)))
(define (store-set-fill-branches S)
  (cond [(empty-store-set? S)  S]
        [else
         (let loop ([S S])
           (cond [(stores? S)  S]
                 [else
                  (match-define (Plain-Store-Set X B L R) S)
                  (store-set X bools (loop L) (loop R))]))]))

(: store-set-join* (-> (Listof Store-Set) (Values Store-Set Boolean)))
(define (store-set-join* Ss)
  (cond [(empty? Ss)  (values empty-store-set #t)]
        [(empty? (rest Ss))  (values (first Ss) #t)]
        [else
         (define m/2 (quotient (length Ss) 2))
         (define-values (S1 S1-exact?) (store-set-join* (take Ss m/2)))
         (define-values (S2 S2-exact?) (store-set-join* (drop Ss m/2)))
         (define-values (S S-exact?) (store-set-join S1 S2))
         (values S (and S-exact? S1-exact? S2-exact?))]))

(: store-set-intersect* (-> (Listof+1 Store-Set) Store-Set))
(define (store-set-intersect* Ss)
  (cond [(empty? (rest Ss))  (first Ss)]
        [else
         (define m/2 (quotient (length Ss) 2))
         (store-set-intersect (store-set-intersect* (assert (take Ss m/2) pair?))
                              (store-set-intersect* (assert (drop Ss m/2) pair?)))]))

(: store-index< (-> Store-Index Store-Index Boolean))
(define (store-index< j0 j1)
  (cond [(empty? j1)  #f]
        [(empty? j0)  #t]
        [(eq? (first j0) (first j1))  (store-index< (rest j0) (rest j1))]
        [(first j1)  #f]
        [else  #t]))

(: store-set-random-indexes (-> Nonempty-Store-Set (Listof Store-Index)))
(define (store-set-random-indexes S)
  (let loop ([S S] [j j0])
    (cond [(stores? S)  empty]
          [else  (match-define (Plain-Store-Set X B L R) S)
                 (define js (append (loop L (cons #t j))
                                    (loop R (cons #f j))))
                 (if (probs? X) js (cons j js))])))

(: store-set-list-random-indexes (-> (Listof Nonempty-Store-Set) (Listof Store-Index)))
(define (store-set-list-random-indexes Ss)
  (sort
   (r.set->list
    (for/fold ([js : (Setof Store-Index)  (r.set)]) ([S  (in-list Ss)])
      (r.set-union js (r.list->set (store-set-random-indexes S)))))
   store-index<))

;; ===================================================================================================
;; Measuring unions of store sets

(: plain-prob-set-endpoints (-> Plain-Prob-Set (Listof Prob)))
(define (plain-prob-set-endpoints X)
  (cond [(Plain-Prob-Interval? X)
         (list (Plain-Prob-Interval-min X)
               (Plain-Prob-Interval-max X))]
        [else
         (append* (map plain-prob-set-endpoints (Prob-Interval-List-elements X)))]))

(: store-set-list-cut (-> (Listof Nonempty-Store-Set) Store-Index
                          (Values (Listof Nonempty-Store-Set)
                                  (Listof Nonempty-Store-Set))))
(define (store-set-list-cut Ss j)
  (define ps
    (sort (append* (map (λ ([S : Nonempty-Store-Set])
                          (define X (store-set-random-proj S j))
                          (if (probs? X) empty (plain-prob-set-endpoints X)))
                        Ss))
          prob<))
  (cond
    [(empty? ps)  (values Ss empty)]
    [else
     (define p-min (first ps))
     (define p-max (last ps))
     (let ([ps  (filter (λ ([p : Prob]) (and (prob< p-min p) (prob< p p-max))) ps)])
       (cond
         [(empty? ps)  (values Ss empty)]
         [else
          (define p (median prob< ps))
          (define X1 (Plain-Prob-Interval prob-0 p #f #f))
          (define X2 (Plain-Prob-Interval p prob-1 #f #f))
          (define S1s (remove-duplicates
                       (append* (map (λ ([S : Nonempty-Store-Set])
                                       (let ([S  (store-set-random-unproj S j X1)])
                                         (if (empty-store-set? S) empty (list S))))
                                     Ss))))
          (define S2s (remove-duplicates
                       (append* (map (λ ([S : Nonempty-Store-Set])
                                       (let ([S  (store-set-random-unproj S j X2)])
                                         (if (empty-store-set? S) empty (list S))))
                                     Ss))))
          (values S1s S2s)]))]))

(: store-set-random-measure/rnd (-> Store-Set Boolean Prob))
(define (store-set-random-measure/rnd S rndu?)
  (cond [rndu?  (store-set-random-measure/rndu S)]
        [else   (store-set-random-measure/rndd S)]))

(: prob+/rnd (-> Prob Prob Boolean (U Prob Bad-Prob)))
(define (prob+/rnd p1 p2 rndu?)
  (if rndu? (prob+/rndu p1 p2) (prob+/rndd p1 p2)))

(: prob-/rnd (-> Prob Prob Boolean (U Prob Bad-Prob)))
(define (prob-/rnd p1 p2 rndu?)
  (if rndu? (prob-/rndu p1 p2) (prob-/rndd p1 p2)))

(: store-set-list-random-measure/rnd (-> (Listof Nonempty-Store-Set) Boolean Prob))
(define (store-set-list-random-measure/rnd Ss rndu?)
  ;; The indexes of every non-full axis in any store set
  (define js (store-set-list-random-indexes Ss))
  (cond
    [(empty? js)  (if (empty? Ss) prob-0 prob-1)]
    [else
     (let loop ([Ss  (remove-duplicates (map store-set-fill-branches Ss))]
                [js  js])
       (cond
         ;; Case: no stores in the union
         [(empty? Ss)  prob-0]
         ;; Case: one store in the union
         [(empty? (rest Ss))  (store-set-random-measure/rnd (first Ss) rndu?)]
         ;; Case: two stores in the union
         [(empty? (rest (rest Ss)))
          ;; P(S1 ∪ S2) = P(S1) + P(S2) - P(S1 ∩ S2) (with rounding)
          (define S1 (first Ss))
          (define S2 (second Ss))
          (define p1 (store-set-random-measure/rnd S1 rndu?))
          (define p2 (store-set-random-measure/rnd S2 rndu?))
          (define q (store-set-random-measure/rnd (store-set-intersect S1 S2) (not rndu?)))
          (define p (let ([p  (prob+/rnd p1 p2 rndu?)]) (if (prob? p) p prob-1)))
          (let ([p  (prob-/rnd p q rndu?)])
            (if (prob? p) p prob-0))]
         ;; Case: many stores in the union, but we've run out of indexes
         [(empty? js)
          ;; It seems like this could only happen if there are a lot of duplicates, and I haven't
          ;; seen it in practice. Better do something sensible anyway...
          (cond
            [rndu?
             ;; Overapproximate (possibly badly)
             (define-values (S S-exact?) (store-set-join* Ss))
             (store-set-random-measure/rndu S)]
            [else
             ;; Underapproximate (possibly badly)
             (define S (store-set-intersect* Ss))
             (store-set-random-measure/rndd S)])]
         ;; Case: we can split the stores in half based on a projection
         [else
          (define-values (S1s S2s) (store-set-list-cut Ss (first js)))
          (cond
            ;; Case: this really shouldn't happen
            [(and (empty? S1s) (empty? S2s))  prob-0]
            ;; Case: one side is empty, so they all have the same projection at this index
            ;; We can leave the index out of all future decisions
            [(empty? S2s)  (loop S1s (rest js))]
            [(empty? S1s)  (loop S2s (rest js))]
            ;; Case: we discriminated!
            [else
             ;; This index may still be worthwhile, so sock it onto the end of the queue
             (define new-js (append (rest js) (list (first js))))
             ;; Measure both sides and add
             (define p1 (loop S1s new-js))
             (define p2 (loop S2s new-js))
             (let ([p  (prob+/rnd p1 p2 rndu?)])
               (if (prob? p) p prob-1))])]))]))

(: store-set-list-random-measure/rndd (-> (Listof Nonempty-Store-Set) Prob))
(define (store-set-list-random-measure/rndd Ss)
  (store-set-list-random-measure/rnd Ss #f))

(: store-set-list-random-measure/rndu (-> (Listof Nonempty-Store-Set) Prob))
(define (store-set-list-random-measure/rndu Ss)
  (store-set-list-random-measure/rnd Ss #t))

;; ===================================================================================================

(: refinement-enumerate (-> Refiner Nonempty-Store-Set Indexes (Listof Nonempty-Store-Set)))
(define (refinement-enumerate refine S idxs)
  (define t (build-search-tree refine S idxs))
  (let loop : (Listof Nonempty-Store-Set) ([t : Store-Search-Tree  t])
    (cond [(search-succ? t)  (define S (search-succ-value t))
                             (cond [(prob-0? (store-set-random-measure S))  empty]
                                   [else  (list S)])]
          [(search-fail? t)  empty]
          [else
           (define S1s (loop (maybe-force (search-node-left t))))
           (define S2s (loop (maybe-force (search-node-right t))))
           (append S1s S2s)])))

;; ===================================================================================================
;; Unconditional probability queries

;; Helper functions

(: enumerate-within (-> (Listof Nonempty-Store-Set) Refiner Indexes Prob (Listof Nonempty-Store-Set)))
;; Refines the enumeration Ss; stops when each part is exact or has measure <= m
(define (enumerate-within Ss refine idxs m)
  (parameterize ([drbayes-refinement-max-splits  16]
                 [drbayes-refinement-prob-min  m])
    (remove-duplicates
     (map store-set-fill-branches
          (append* (map (λ ([S : Nonempty-Store-Set])
                          (refinement-enumerate refine S idxs))
                        Ss))))))

(: adjust-probability (-> Prob (Listof Nonempty-Store-Set) (Listof Nonempty-Store-Set) Prob))
;; Given probability upper bound p of enumeration Ss, computes a lesser upper bound for a finer
;; enumeration new-Ss
;; This works correctly even when Ss (and thus new-Ss) lacks its interior parts
(define (adjust-probability p Ss new-Ss)
  (let* ([q  (prob-/rndd (store-set-list-random-measure/rndd Ss)
                         (store-set-list-random-measure/rndu new-Ss))]
         [q  (if (prob? q) q prob-0)]
         [p  (prob-/rndu p q)])
    (if (prob? p) p prob-0)))

(: cull-interior (-> (Listof Nonempty-Store-Set) Refiner Indexes (Listof Nonempty-Store-Set)))
;; Given an enumeration Ss and a refiner for its *complement*, removes the interior parts of Ss
(define (cull-interior Ss refine idxs)
  (remove-duplicates
   (for/fold ([new-Ss : (Listof Nonempty-Store-Set)  empty])
             ([S  (in-list Ss)])
     (let*-values ([(S idxs)  (refine-ifte*-indexes refine S idxs)]
                   [(S S-exact?)  (refine S)]
                   [(S)  (store-set-fill-branches S)])
       (if (empty-store-set? S) new-Ss (cons S new-Ss))))))

;; Return parameters

(: drbayes-query-t-stores (Parameterof (Listof (Listof Nonempty-Store-Set))))
(define drbayes-query-t-stores (make-parameter empty))

(: drbayes-query-f-stores (Parameterof (Listof (Listof Nonempty-Store-Set))))
(define drbayes-query-f-stores (make-parameter empty))

;; The query function itself

(: drbayes-query (->* [meaning] [Natural] (Values Prob Prob Prob)))
;; Computes bounds on the probability that e outputs #t
(define (drbayes-query e [steps 100])
  ;; Extract what we need from the interpretation of the program
  (match-define (meaning _ _ (app run/pre* h) k) e)
  (define t-refine (make-preimage-refiner h trues))   ; Refiner for the preimage of {true}
  (define f-refine (make-preimage-refiner h falses))  ; Refiner for the preimage of {false}
  (define idxs (k j0))
  
  (define s (drbayes-enumerate-relative-prob-min))
  
  (: pt Prob)
  (: pf Prob)
  (: t-stores (Listof (Listof Nonempty-Store-Set)))
  (: f-stores (Listof (Listof Nonempty-Store-Set)))
  (define-values (pt pf t-stores f-stores)
    (let loop ([t-stores : (Listof (Listof Nonempty-Store-Set))  empty]
               [f-stores : (Listof (Listof Nonempty-Store-Set))  empty]
               [t-Ss : (Listof Nonempty-Store-Set)  (list stores)]  ; preimage of {true}
               [f-Ss : (Listof Nonempty-Store-Set)  (list stores)]  ; preimage of {false}
               [pt : Prob  prob-1]  ; current upper bound on probability of {true}
               [pf : Prob  prob-1]  ; current upper bound on probability of {false}
               [m : Prob  (drbayes-enumerate-prob-min)]  ; minimum probability of a part
               [i : Natural  0])  ; iteration #
      (cond
        [(>= i steps)  (values pt pf t-stores f-stores)]
        [else
         (printf "~nIteration ~v~n" i)
         (printf "  Refining enumerations with minimum probability ~v...~n" (prob->flonum m))
         (define new-t-Ss (enumerate-within t-Ss t-refine idxs m))
         (define new-f-Ss (enumerate-within f-Ss f-refine idxs m))
         (printf "    Enumeration sizes: ~v ~v~n" (length new-t-Ss) (length new-f-Ss))
         (printf "  Adjusting enumeration probability bounds...~n")
         ;; Adjust probabilities to account for points not included in finer enumeration
         (define new-pt (adjust-probability pt t-Ss new-t-Ss))
         (define new-pf (adjust-probability pf f-Ss new-f-Ss))
         (printf "    Enumeration bounds: ~v ~v~n" (prob->flonum new-pt) (prob->flonum new-pf))
         (printf "    (Query bounds: ~v ~v)~n"  (prob->flonum (prob1- new-pf)) (prob->flonum new-pt))
         (printf "  Culling enumeration interiors using complement refiners...~n")
         (define new-t-Ss* (cull-interior new-t-Ss f-refine idxs))
         (define new-f-Ss* (cull-interior new-f-Ss t-refine idxs))
         (printf "    Enumeration sizes: ~v ~v~n" (length new-t-Ss*) (length new-f-Ss*))
         
         (define new-t-stores (list* new-t-Ss* new-t-Ss t-stores))
         (define new-f-stores (list* new-f-Ss* new-f-Ss f-stores))
         
         (define new-m (prob*/rndu s (prob-max m (prob-min new-pt new-pf))))
         (cond [(prob>= new-m m)
                (printf "~nStopping: new minimum probability ~v >= ~v~n"
                        (prob->flonum new-m)
                        (prob->flonum m))
                (values pt pf new-t-stores new-f-stores)]
               [else
                (loop new-t-stores new-f-stores
                      new-t-Ss* new-f-Ss*
                      new-pt new-pf
                      new-m
                      (+ i 1))])])))
  
  (define p-max pt)
  (define p-min (prob1- pf))
  (define p-mid (prob-midpoint p-min p-max))
  
  (drbayes-query-t-stores (reverse t-stores))
  (drbayes-query-f-stores (reverse f-stores))
  (values p-min p-mid p-max))

;; ===================================================================================================
;; Conditional probability queries

(: drbayes-cond-query-tt-stores (Parameterof (Listof (Listof Nonempty-Store-Set))))
(define drbayes-cond-query-tt-stores (make-parameter empty))

(: drbayes-cond-query-!tt-stores (Parameterof (Listof (Listof Nonempty-Store-Set))))
(define drbayes-cond-query-!tt-stores (make-parameter empty))

(: drbayes-cond-query-ft-stores (Parameterof (Listof (Listof Nonempty-Store-Set))))
(define drbayes-cond-query-ft-stores (make-parameter empty))

(: drbayes-cond-query-!ft-stores (Parameterof (Listof (Listof Nonempty-Store-Set))))
(define drbayes-cond-query-!ft-stores (make-parameter empty))

(: drbayes-cond-query (->* [meaning] [Natural] (Values Prob Prob Prob)))
(define (drbayes-cond-query e [steps 100])
  (parameterize ([drbayes-query-t-stores  empty]
                 [drbayes-query-f-stores  empty])
    
    (define tt-e (drbayes (let ([x ,e]) (and (car x) (cdr x)))))
    (define ft-e (drbayes (let ([x ,e]) (and (not (car x)) (cdr x)))))
    
    (define-values (tt-min tt-mid tt-max) (drbayes-query tt-e steps))
    (define tt-Sss (drbayes-query-t-stores))
    (define !tt-Sss (drbayes-query-f-stores))
    
    (printf "~nPr[<true,true>] ∈ [~v, ~v]~n" (prob->flonum tt-min) (prob->flonum tt-max))
    (printf "Pr[<true,true>] ≈ ~v~n" (prob->flonum tt-mid))
    
    (define-values (ft-min ft-mid ft-max) (drbayes-query ft-e steps))
    (define ft-Sss (drbayes-query-t-stores))
    (define !ft-Sss (drbayes-query-f-stores))
    
    (printf "~nPr[<false,true>] ∈ [~v, ~v]~n" (prob->flonum ft-min) (prob->flonum ft-max))
    (printf "Pr[<false,true>] ≈ ~v~n" (prob->flonum ft-mid))
    
    (define-values (p-min p-max) (prob-normalize-first/ivl tt-min tt-max ft-min ft-max))
    (define p-mid (prob-normalize-first tt-mid ft-mid))
    
    (drbayes-cond-query-tt-stores tt-Sss)
    (drbayes-cond-query-!tt-stores !tt-Sss)
    (drbayes-cond-query-ft-stores ft-Sss)
    (drbayes-cond-query-!ft-stores !ft-Sss)
    (values p-min p-mid p-max)))
