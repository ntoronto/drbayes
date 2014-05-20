#lang typed/racket/base

(require racket/match
         racket/list
         "../types.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(define-singleton-type Empty-Set empty-set)
(define-singleton-type Univ-Set univ-set)

(define-singleton-type Null-Set null-set)

(define-type Nonempty-Set (U Univ-Set Real-Set Bool-Set Pair-Set Null-Set Tree-Set))
(define-type Set (U Empty-Set Nonempty-Set))

(define-type Value (Rec V (U Flonum Boolean (Pair V V) Tree-Val)))

(: set-meet (Set Set -> Set))
(define (set-meet A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-meet A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-meet A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-meet A B)]
        [(and (null-set? A) (null-set? B))  null-set]
        [(and (tree-set? A) (tree-set? B))  (tree-set-meet A B)]
        [(univ-set? A)  B]
        [(univ-set? B)  A]
        [else  empty-set]))

(: set-join (case-> (Nonempty-Set Nonempty-Set -> Nonempty-Set)
                    (Set Set -> Set)))
(define (set-join A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-join A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-join A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-join A B)]
        [(and (null-set? A) (null-set? B))  null-set]
        [(and (tree-set? A) (tree-set? B))  (tree-set-join A B)]
        [(or (univ-set? A) (univ-set? B))  univ-set]
        [(empty-set? A)  B]
        [(empty-set? B)  A]
        [else  univ-set]))

(: set-member? (Set Value -> Boolean))
(define (set-member? A a)
  (cond [(and (real-set? A) (flonum? a))    (real-set-member? A a)]
        [(and (bool-set? A) (boolean? a))   (bool-set-member? A a)]
        [(and (pair-set? A) (pair? a))      (pair-set-member? A a)]
        [(and (null-set? A) (null? a))      #t]
        [(and (tree-set? A) (tree-val? a))  (tree-set-member? A a)]
        [(univ-set? A)  #t]
        [else  #f]))

(: set-singleton (Value -> Nonempty-Set))
(define (set-singleton a)
  (cond [(flonum? a)    (real-set-singleton a)]
        [(boolean? a)   (bool-set-singleton a)]
        [(pair? a)      (pair-set-singleton a)]
        [(null? a)      null-set]
        [(tree-val? a)  (tree-set-singleton a)]))

(: set-proj-fst (Set -> Set))
(define (set-proj-fst A)
  (cond [(pair-set? A)  (pair-set-proj-fst A)]
        [(univ-set? A)  (pair-set-proj-fst univ-pair-set)]
        [else  empty-set]))

(: set-proj-snd (Set -> Set))
(define (set-proj-snd A)
  (cond [(pair-set? A)  (pair-set-proj-snd A)]
        [(univ-set? A)  (pair-set-proj-snd univ-pair-set)]
        [else  empty-set]))

(: set-project (Tree-Index Set -> Set))
(define (set-project j A)
  (cond [(tree-set? A)  (tree-set-project j A)]
        [(univ-set? A)  (tree-set-project j univ-tree-set)]
        [else  empty-set]))

(: set-unproject (Tree-Index Set Set -> Set))
(define (set-unproject j A B)
  (cond [(tree-set? A)  (tree-set-unproject j A B)]
        [(univ-set? A)  (tree-set-unproject j univ-tree-set B)]
        [else  empty-set]))

;; ---------------------------------------------------------------------------------------------------
;; Reals

(struct: Real-Set-Base () #:transparent)
(struct: interval Real-Set-Base ([min : Flonum] [max : Flonum]) #:transparent)

(define-type Real-Set (U interval))
(define real-set? Real-Set-Base?)

(define real-ivl (interval -inf.0 +inf.0))
(define unit-ivl (interval 0.0 1.0))

(: ivl (Flonum Flonum -> (U Empty-Set Real-Set)))
(define (ivl min max)
  (cond [(min . <= . max)  (interval min max)]
        [else  empty-set]))

(: real-set-meet (Real-Set Real-Set -> (U Empty-Set Real-Set)))
(define (real-set-meet A B)
  (match-define (interval amin amax) A)
  (match-define (interval bmin bmax) B)
  (ivl (max amin bmin) (min amax bmax)))

(: real-set-join (Real-Set Real-Set -> Real-Set))
(define (real-set-join A B)
  (match-define (interval amin amax) A)
  (match-define (interval bmin bmax) B)
  (interval (min amin bmin) (max amax bmax)))

(: real-set-member? (Real-Set Flonum -> Boolean))
(define (real-set-member? A a)
  (match-define (interval amin amax) A)
  (<= amin a amax))

(: real-set-singleton (Flonum -> Real-Set))
(define (real-set-singleton a)
  (interval a a))

;; ---------------------------------------------------------------------------------------------------
;; Booleans

(struct: Bool-Set-Base () #:transparent)
(define-singleton-type True-Set Bool-Set-Base true-set)
(define-singleton-type False-Set Bool-Set-Base false-set)
(define-singleton-type Bools-Set Bool-Set-Base bools-set)

(define-type Bool-Set (U True-Set False-Set Bools-Set))
(define bool-set? Bool-Set-Base?)

(: bool-set-meet (Bool-Set Bool-Set -> (U Empty-Set Bool-Set)))
(define (bool-set-meet A B)
  (cond [(bools-set? A)  B]
        [(bools-set? B)  A]
        [(eq? A B)  A]
        [else  empty-set]))

(: bool-set-join (Bool-Set Bool-Set -> Bool-Set))
(define (bool-set-join A B)
  (cond [(or (bools-set? A) (bools-set? B))  bools-set]
        [(eq? A B)  A]
        [else  bools-set]))

(: bool-set-member? (Bool-Set Boolean -> Boolean))
(define (bool-set-member? A a)
  (cond [(bools-set? A)   #t]
        [else  (eq? (true-set? A) a)]))

(: bool-set-singleton (Boolean -> Bool-Set))
(define (bool-set-singleton a)
  (if a true-set false-set))

;; ---------------------------------------------------------------------------------------------------
;; Pairs

(struct: Pair-Set-Base () #:transparent)
(struct: prod-set Pair-Set-Base ([fst : Nonempty-Set] [snd : Nonempty-Set]) #:transparent)

(define-type Pair-Set (U prod-set))
(define pair-set? Pair-Set-Base?)

(define univ-pair-set (prod-set univ-set univ-set))

(: set-prod (case-> (Nonempty-Set Nonempty-Set -> Pair-Set)
                    (Set Set -> (U Empty-Set Pair-Set))))
(define (set-prod A1 A2)
  (cond [(empty-set? A1)  empty-set]
        [(empty-set? A2)  empty-set]
        [else  (prod-set A1 A2)]))

(: pair-set-proj-fst (Pair-Set -> Nonempty-Set))
(define pair-set-proj-fst prod-set-fst)

(: pair-set-proj-snd (Pair-Set -> Nonempty-Set))
(define pair-set-proj-snd prod-set-snd)

(: pair-set-meet (Pair-Set Pair-Set -> (U Empty-Set Pair-Set)))
(define (pair-set-meet A B)
  (set-prod (set-meet (prod-set-fst A) (prod-set-fst B))
            (set-meet (prod-set-snd A) (prod-set-snd B))))

(: pair-set-join (Pair-Set Pair-Set -> Pair-Set))
(define (pair-set-join A B)
  (prod-set (set-join (prod-set-fst A) (prod-set-fst B))
            (set-join (prod-set-snd A) (prod-set-snd B))))

(: pair-set-member? (Pair-Set (Pair Value Value) -> Boolean))
(define (pair-set-member? A a)
  (and (set-member? (prod-set-fst A) (car a))
       (set-member? (prod-set-snd A) (cdr a))))

(: pair-set-singleton ((Pair Value Value) -> Pair-Set))
(define (pair-set-singleton a)
  (prod-set (set-singleton (car a)) (set-singleton (cdr a))))

;; ===================================================================================================
;; Sets of countable vectors with at most finitely many restricted axes

(define-type Tree-Index (Listof Boolean))

(: index-left (Tree-Index -> Tree-Index))
(define (index-left j) (cons #t j))

(: index-right (Tree-Index -> Tree-Index))
(define (index-right j) (cons #f j))

(define j0 '())

;; ---------------------------------------------------------------------------------------------------

(struct: Tree-Val-Base () #:transparent)
(struct: tree-val-node Tree-Val-Base ([value : Value] [left : Tree-Val] [right : Tree-Val])
  #:transparent)
(define-singleton-type Any-Tree-Val Tree-Val-Base any-tree-val)

(define-type Tree-Val (U Any-Tree-Val tree-val-node))
(define tree-val? Tree-Val-Base?)

(: tree-val-ref (All (X) (Tree-Index Tree-Val -> Value)))
(define (tree-val-ref j t)
  (define j0 j)
  (let loop ([j j] [t t])
    (match t
      [(? any-tree-val?)  (error 'tree-val-ref "indeterminate value at ~e" j0)]
      [(tree-val-node x l r)  (cond [(empty? j)  x]
                                    [(first j)  (loop (rest j) l)]
                                    [else       (loop (rest j) r)])])))

;; ---------------------------------------------------------------------------------------------------

(struct: Base-Tree-Set () #:transparent)
(struct: Tree-Set-Node Base-Tree-Set ([axis : Nonempty-Set] [left : Tree-Set] [right : Tree-Set])
  #:transparent)
(define-singleton-type Univ-Tree-Set Base-Tree-Set univ-tree-set)

(define-type Tree-Set (U Tree-Set-Node Univ-Tree-Set))
(define tree-set? Base-Tree-Set?)

(: nonempty-tree-set-node (Nonempty-Set Tree-Set Tree-Set -> Tree-Set))
(define (nonempty-tree-set-node A L R)
  (cond [(and (univ-set? A) (univ-tree-set? L) (univ-tree-set? R))  univ-tree-set]
        [else  (Tree-Set-Node A L R)]))

(: tree-set-node (Set (U Empty-Set Tree-Set) (U Empty-Set Tree-Set) -> (U Empty-Set Tree-Set)))
(define (tree-set-node A L R)
  (cond [(or (empty-set? A) (empty-set? L) (empty-set? R))  empty-set]
        [else  (nonempty-tree-set-node A L R)]))

(: tree-set-meet (Tree-Set Tree-Set -> (U Empty-Set Tree-Set)))
(define (tree-set-meet A1 A2)
  (cond [(univ-tree-set? A1)  A2]
        [(univ-tree-set? A2)  A1]
        [else  (match-let ([(Tree-Set-Node A1 L1 R1)  A1]
                           [(Tree-Set-Node A2 L2 R2)  A2])
                 (tree-set-node (set-meet A1 A2) (tree-set-meet L1 L2) (tree-set-meet R1 R2)))]))

(: tree-set-join (Tree-Set Tree-Set -> Tree-Set))
(define (tree-set-join A1 A2)
  (cond [(or (univ-tree-set? A1) (univ-tree-set? A2))  univ-tree-set]
        [else  (match-let ([(Tree-Set-Node A1 L1 R1)  A1]
                           [(Tree-Set-Node A2 L2 R2)  A2])
                 (nonempty-tree-set-node (set-join A1 A2)
                                         (tree-set-join L1 L2)
                                         (tree-set-join R1 R2)))]))

(: tree-set-member? (Tree-Set Tree-Val -> Boolean))
(define (tree-set-member? A a)
  (cond [(univ-tree-set? A)   #t]
        [(any-tree-val? a)  (error 'tree-set-member? "indeterminate value")]
        [else  (match-let ([(Tree-Set-Node A L R)  A]
                           [(tree-val-node a l r)  a])
                 (and (set-member? A a) (tree-set-member? L l) (tree-set-member? R r)))]))

(: tree-set-singleton (Tree-Val -> Tree-Set))
(define (tree-set-singleton a)
  (match a
    [(? any-tree-val?)  univ-tree-set]
    [(tree-val-node a l r)  (Tree-Set-Node (set-singleton a)
                                           (tree-set-singleton l)
                                           (tree-set-singleton r))]))

(: tree-set-project (Tree-Index Tree-Set -> Set))
(define (tree-set-project j A)
  (cond [(univ-tree-set? A)   univ-set]
        [(empty? j)  (Tree-Set-Node-axis A)]
        [(first j)   (tree-set-project (rest j) (Tree-Set-Node-left A))]
        [else        (tree-set-project (rest j) (Tree-Set-Node-right A))]))

(: tree-set-unproject (Tree-Index Tree-Set Set -> (U Empty-Set Tree-Set)))
(define (tree-set-unproject j A B)
  (let loop ([j j] [A A])
    (cond [(univ-tree-set? A)   (loop j (Tree-Set-Node univ-set univ-tree-set univ-tree-set))]
          [else  (match-let ([(Tree-Set-Node A L R)  A])
                   (cond [(empty? j)  (tree-set-node (set-meet A B) L R)]
                         [(first j)   (tree-set-node A (loop (rest j) L) R)]
                         [else        (tree-set-node A L (loop (rest j) R))]))])))
