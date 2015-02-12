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

(struct (S) search-succ ([value : S] [prob : Prob]) #:transparent)
(struct (F) search-fail ([value : F]) #:transparent)
(struct (S F) search-node ([left : (Promise (Search-Tree S F))]
                           [right : (Promise (Search-Tree S F))]
                           [prob-left : Prob]
                           [name : Symbol])
  #:transparent)

(define-type (Search-Tree S F) (U (search-succ S)
                                  (search-fail F)
                                  (search-node S F)))

;; ===================================================================================================

(: search-tree-size (All (S F) (-> (Search-Tree S F) Natural)))
(define (search-tree-size t)
  (match t
    [(search-node cl cr pl name)
     (for/fold ([n : Natural  0]) ([c  (in-list (list cl cr))])
       (if (or (not (promise? c)) (promise-forced? c))
           (+ n (search-tree-size (force c)))
           n))]
    [_  1]))

(: search-tree-abstract (All (S F) (-> (Search-Tree S F) Any)))
(define (search-tree-abstract t)
  (match t
    [(search-fail _)  '(search-fail F)]
    [(search-succ _ p)  '(search-succ S)]
    [(search-node cl cr pl name)
     (define cl* (if (promise-forced? cl) (search-tree-abstract (force cl)) 'promise))
     (define cr* (if (promise-forced? cr) (search-tree-abstract (force cr)) 'promise))
     (list 'search-node cl* cr* (prob->flonum pl) name)]))

;; ===================================================================================================
;; Adaptive sampling
;; Transliterated from Figure 9.22 in the dissertation

(: adjusted-node (All (S F) (-> Prob
                                (Promise (Search-Tree S F))
                                (Promise (Search-Tree S F))
                                Prob
                                Prob
                                Symbol
                                (Values Prob (Search-Tree S F)))))
(define (adjusted-node pt cl cr pl pr name)
  (cond [(or (prob-0? pl) (prob-1? pr))  (values (prob* pt pr) (force cr))]
        [(or (prob-0? pr) (prob-1? pl))  (values (prob* pt pl) (force cl))]
        [else
         (values (let ([pt  (prob+ (prob* pt pl) (prob* pt pr))])
                   (if (prob? pt) pt prob-1))
                 (search-node cl cr (prob-normalize-first pl pr) name))]))

(: sample-search-tree (All (S F) (-> Prob (Search-Tree S F)
                                     (Values Prob (U S F)
                                             Prob (Search-Tree S F)))))
(define (sample-search-tree pt t)
  (match t
    [(search-succ x px)
     ;; Removes zero-probability leaves without altering other leaf probabilities:
     ;(values pt x pt t)
     ;; Convergence properties not known, but it seems to work well, and it's necessary when
     ;; drbayes-always-terminate? is #t
     (values pt x (prob-min px pt) t)
     ]
    [(search-fail x)  (values pt x prob-0 t)]
    [(search-node cl cr pl name)
     (when search-stats? (increment-search-stat name))
     (define pr (prob1- pl))
     (define left? (prob-boolean pl))
     (let*-values ([(cl cr pl pr)  (if left? (values cl cr pl pr) (values cr cl pr pl))]
                   [(px x pt* cl*)  (sample-search-tree (prob* pt pl) (force cl))]
                   [(pl*)  (prob/ pt* pt)]
                   [(pl*)  (if (prob? pl*) pl* prob-1)]
                   [(cl*)  (delay cl*)]
                   [(_)  (force cl*)]
                   [(pt t)  (if left?
                                (adjusted-node pt cl* cr pl* pr name)
                                (adjusted-node pt cr cl* pr pl* name))])
       (values px x pt t))]))
