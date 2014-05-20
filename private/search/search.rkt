#lang typed/racket/base

(require racket/match
         racket/list
         math/flonum
         "../utils.rkt"
         "../untyped-utils.rkt")

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
;; Search tree types

(struct: (X) search-leaf ([value : X] [prob : Flonum]) #:transparent)

(struct: (X) search-node ([children : (Listof+2 (Maybe-Promise (Search-Tree X)))]
                          [probs : (Listof+2 Flonum)]
                          [choice : (U 'nondeterministic 'probabilistic)]
                          [name : Symbol])
  #:transparent)

(define-type (Search-Tree X) (U (search-leaf X) (search-node X)))

;; ===================================================================================================
;; Nonadaptive, independent sampling

(: sample-search-tree-once (All (X) ((Search-Tree X) Flonum -> (Values (Listof (search-leaf X))
                                                                       (Listof Flonum)))))
;; Straightforward sampler, useful only for rejection sampling
;; Returns a leaf in the search tree, and the probability of returning that leaf
(define (sample-search-tree-once t p)
  (cond
    [(search-node? t)
     (match-define (search-node cs qs choice name) t)
     (when search-stats? (increment-search-stat name))
     (cond [#t #;(eq? choice 'probabilistic)
            (define i (sample-index qs))
            (define c (maybe-force (list-ref cs i)))
            (define q (list-ref qs i))
            (sample-search-tree-once c (* p q))]
           [(eq? choice 'nondeterministic)
            (define: ts : (Listof (search-leaf X))  empty)
            (define: ps : (Listof Flonum)  empty)
            (let loop ([cs cs] [ts ts] [ps ps])
              (cond [(empty? cs)  (values ts ps)]
                    [else
                     (define-values (new-ts new-ps)
                       (sample-search-tree-once (maybe-force (first cs)) p))
                     (loop (rest cs) (append new-ts ts) (append new-ps ps))]))])]
    [else
     (values (list t) (list p))]))

(: sample-search-tree-once* (All (X) ((Search-Tree X) Integer -> (Values (Listof (search-leaf X))
                                                                         (Listof Flonum)))))
;; Samples multiple leaves using `sample-search-tree-once'
(define (sample-search-tree-once* t n)
  (cond
    [(not (index? n))   (raise-argument-error 'sample-search-tree-once* "Index" 1 t n)]
    [else
     (define: ts : (Listof (search-leaf X))  empty)
     (define: ps : (Listof Flonum)  empty)
     (let: loop ([i : Positive-Fixnum  1] [ts ts] [ps ps])
       (cond [(i . <= . n)
              (when (= 0 (remainder i 100))
                (printf "i = ~v~n" i)
                (flush-output))
              (let-values ([(leaf-ts leaf-ps)  (sample-search-tree-once t 1.0)])
                (loop (+ i 1) (append leaf-ts ts) (append leaf-ps ps)))]
             [else
              (values ts ps)]))]))

;; ===================================================================================================
;; Adaptive search

(: sample-search-tree (All (X) ((Search-Tree X) Flonum -> (Values (Listof (search-leaf X))
                                                                  (Listof Flonum)
                                                                  (Search-Tree X)
                                                                  Nonnegative-Flonum))))
;; Like `sample-search-tree-once', but returns an adjusted tree in which the probability of choosing
;; the leaf is the leaf value's actual probability
(define (sample-search-tree t p)
  ;(define-values (leaf-ts leaf-ps) (sample-search-tree-once t p))
  ;(values leaf-ts leaf-ps t p)
  (cond
    [(search-node? t)
     (match-define (search-node cs qs choice name) t)
     (when search-stats? (increment-search-stat name))
     (define i (sample-index qs))
     (define c (maybe-force (list-ref cs i)))
     (define q (list-ref qs i))
     (define pq (* p q))
     (let*-values
         ([(leaf-ts leaf-ps c child-pq)  (sample-search-tree c pq)]
          [(child-q)  (max 0.0 (/ child-pq p))]
          [(t)  (cond [(zero? child-q)
                       (let* ([cs  (remove-index/+2 cs i)]
                              [qs  (remove-index/+2 qs i)])
                         (cond [(or (empty? (rest cs)) (empty? (rest qs)))
                                (maybe-force (first cs))]
                               [else
                                ;(printf "normalizing ~v probabilities~n" (length qs))
                                (let ([qs  (normalize-probs/+2 qs)])
                                  (search-node cs qs choice name))]))]
                      [else
                       (let* ([cs  (list-set/+2 cs i c)]
                              [qs  (list-set/+2 qs i child-q)]
                              ;[_   (printf "normalizing ~v probabilities~n" (length qs))]
                              [qs  (normalize-probs/+2 qs)])
                         (search-node cs qs choice name))])])
       (values leaf-ts leaf-ps t (max 0.0 (* p (- 1.0 (- q child-q))))))]
    [else
     (define leaf-p (search-leaf-prob t))
     (values (list t) (list p)
             t
             #;(if (zero? leaf-p) 0.0 (max 0.0 p))
             (max 0.0 (min p leaf-p)))]))

(: sample-search-tree* (All (X) ((Search-Tree X) Integer -> (Values (Listof (search-leaf X))
                                                                    (Listof Flonum)
                                                                    (Search-Tree X)
                                                                    Flonum))))
;; Samples multiple leaves using `sample-search-tree'
(define (sample-search-tree* t n)
  (cond
    [(not (index? n))  (raise-argument-error 'sample-search-tree* "Index" 1 t n)]
    [else
     (define: ts : (Listof (search-leaf X))  empty)
     (define: ps : (Listof Flonum)  empty)
     (let: loop ([i : Positive-Fixnum  1] [ts ts] [ps ps] [t t] [q 1.0])
       (cond [(and (i . <= . n) (q . > . 0.0))
              (let-values ([(leaf-ts leaf-ps t q)  (sample-search-tree t q)])
                (when (= 0 (remainder i 100))
                  (printf "i = ~v~n" i)
                  (flush-output))
                (loop (+ i 1) (append leaf-ts ts) (append leaf-ps ps) t q))]
             [else
              (when (q . <= . 0.0)
                (printf "bailing because top probability is zero~n"))
              (values ts ps t q)]))]))
