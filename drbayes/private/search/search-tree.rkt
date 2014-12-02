#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "../flonum.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt"
         "utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Statistics

(define search-stats? #t)

(define: search-stats : (HashTable Symbol Natural)  (make-hasheq empty))

(: increment-search-stat (Symbol -> Void))
(define (increment-search-stat name)
  (hash-set! search-stats name (+ 1 (hash-ref search-stats name (λ () 0)))))

(define (reset-search-stats)
  (set! search-stats ((inst make-hasheq Symbol Natural) empty)))

(: get-search-stats (-> (Listof (Pair Symbol Natural))))
(define (get-search-stats)
  ((inst sort (Pair Symbol Natural) String)
   (hash-map search-stats (λ: ([k : Symbol] [v : Natural]) (cons k v)))
   string<?
   #:key (λ: ([kv : (Pair Symbol Natural)]) (symbol->string (car kv)))
   #:cache-keys? #t))

;; ===================================================================================================

(struct: (X) search-leaf ([value : X] [prob : Prob]) #:transparent)

(struct: (X) search-node ([fst : (Maybe-Promise (Search-Tree X))]
                          [snd : (Maybe-Promise (Search-Tree X))]
                          [prob-fst : Prob]
                          [name : Symbol])
  #:transparent)

(define-type (Search-Tree X) (U (search-leaf X) (search-node X)))

(: make-search-node (All (X) (-> (Maybe-Promise (Search-Tree X))
                                 (Maybe-Promise (Search-Tree X))
                                 Prob
                                 Symbol
                                 (Search-Tree X))))
(define (make-search-node c1 c2 q1 name)
  (cond [(prob-1? q1)  (maybe-force c1)]
        [(prob-0? q1)  (maybe-force c2)]
        [else  (search-node c1 c2 q1 name)]))

;; ===================================================================================================

(: search-tree-size (All (X) (-> (Search-Tree X) Natural)))
(define (search-tree-size t)
  (cond
    [(search-node? t)
     (match-define (search-node c1 c2 q1 name) t)
     (for/fold ([n : Natural  0]) ([c  (in-list (list c1 c2))])
       (if (or (not (promise? c)) (promise-forced? c))
           (+ n (search-tree-size (maybe-force c)))
           n))]
    [else  1]))

;; ===================================================================================================
;; Nonadaptive, independent sampling

(: sample-search-tree-once (All (X) (-> (Search-Tree X) Prob (Values (search-leaf X) Prob))))
;; Straightforward sampler, useful only for rejection sampling
;; Returns a leaf in the search tree, and the probability of returning that leaf
(define (sample-search-tree-once t p)
  (cond
    [(search-node? t)
     (match-define (search-node c1 c2 q1 name) t)
     (when search-stats? (increment-search-stat name))
     (let-values ([(c q)  (if (prob-boolean q1) (values c1 q1) (values c2 (prob1- q1)))])
       (sample-search-tree-once (maybe-force c) (prob* p q)))]
    [else
     (values t p)]))

(: sample-search-tree-once* (All (X) (-> (Search-Tree X) Integer (Values (Listof (search-leaf X))
                                                                         (Listof Prob)))))
;; Samples multiple leaves using `sample-search-tree-once'
(define (sample-search-tree-once* t n)
  (cond
    [(not (index? n))   (raise-argument-error 'sample-search-tree-once* "Index" 1 t n)]
    [else
     (define: ts : (Listof (search-leaf X))  empty)
     (define: ps : (Listof Prob)  empty)
     (let: loop ([i : Positive-Fixnum  1] [ts ts] [ps ps])
       (cond [(i . <= . n)
              (when (= 0 (remainder i 100))
                (printf "i = ~v~n" i)
                (flush-output))
              (let-values ([(leaf-t leaf-p)  (sample-search-tree-once t prob-1)])
                (loop (+ i 1) (cons leaf-t ts) (cons leaf-p ps)))]
             [else
              (values ts ps)]))]))

;; ===================================================================================================
;; Adaptive search

(: sample-search-tree (All (X) (-> (Search-Tree X) Prob (Values (search-leaf X) Prob
                                                                (Search-Tree X) Prob))))
;; Like `sample-search-tree-once', but returns an adjusted tree in which the probability of choosing
;; the leaf is the leaf value's actual probability
(define (sample-search-tree t p)
  ;(define-values (leaf-ts leaf-ps) (sample-search-tree-once t p))
  ;(values leaf-ts leaf-ps t p)
  (cond
    [(search-node? t)
     (match-define (search-node c1 c2 q1 name) t)
     (when search-stats? (increment-search-stat name))
     (define fst? (prob-boolean q1))
     (let-values ([(c1 c2 q1 q2)  (cond [fst?  (values (maybe-force c1) c2 q1 (prob1- q1))]
                                        [else  (values (maybe-force c2) c1 (prob1- q1) q1)])])
       (define pq1 (prob* p q1))
       (let*-values ([(leaf-ts leaf-ps c1 pq1)  (sample-search-tree c1 pq1)]
                     [(new-q1)  (prob-normalize-first (assert (prob/ pq1 p) prob?) q2)]
                     [(t)  (make-search-node c1 c2 new-q1 name)])
         (define q (let ([q  (prob- q1 new-q1)])
                     (if (prob? q) q prob-0)))
         (values leaf-ts leaf-ps t (prob* p (prob1- q)))))]
    [else
     (define leaf-p (search-leaf-prob t))
     (values t p
             t
             (prob-min p leaf-p)
             ;(if (prob-0? leaf-p) prob-0 p)
             )]))

(: sample-search-tree* (All (X) (-> (Search-Tree X) Integer (Values (Listof (search-leaf X))
                                                                    (Listof Prob)
                                                                    (Search-Tree X)
                                                                    Prob))))
;; Samples multiple leaves using `sample-search-tree'
(define (sample-search-tree* t n)
  (cond
    [(not (index? n))  (raise-argument-error 'sample-search-tree* "Index" 1 t n)]
    [else
     (define: ts : (Listof (search-leaf X))  empty)
     (define: ps : (Listof Prob)  empty)
     (let: loop ([i : Positive-Fixnum  1] [ts ts] [ps ps] [t t] [q prob-1])
       (cond [(and (i . <= . n) (not (prob-0? q)))
              (let-values ([(leaf-t leaf-p t q)  (sample-search-tree t q)])
                (when (= 0 (remainder i 100))
                  (printf "i = ~v~n" i)
                  (flush-output))
                (loop (+ i 1) (cons leaf-t ts) (cons leaf-p ps) t q))]
             [else
              (when (prob-0? q)
                (printf "bailing because top probability is zero~n"))
              (values ts ps t q)]))]))
