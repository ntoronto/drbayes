#lang typed/racket/base

(require "../untyped-utils.rkt"
         "set.rkt"
         racket/list
         racket/match)

(provide (all-defined-out))

;; ---------------------------------------------------------------------------------------------------

(define-type Tree-Index (Listof Boolean))

(: index-left (Tree-Index -> Tree-Index))
(define (index-left j) (cons #t j))

(: index-right (Tree-Index -> Tree-Index))
(define (index-right j) (cons #f j))

(define j0 '())

;; ---------------------------------------------------------------------------------------------------

(define-singleton-type Any-Tree-Val any-tree-val)
(struct: (X) tree-val-node ([value : X] [left : (Tree-Val X)] [right : (Tree-Val X)])
  #:transparent)

(define-type (Tree-Val X) (U Any-Tree-Val (tree-val-node X)))

(: tree-val-ref (All (X) (Tree-Index (Tree-Val X) -> X)))
(define (tree-val-ref j t)
  (define j0 j)
  (let loop ([j j] [t t])
    (match t
      [(? any-tree-val?)  (error 'tree-val-ref "indeterminate value at ~e" j0)]
      [(tree-val-node x l r)  (cond [(empty? j)  x]
                                    [(first j)  (loop (rest j) l)]
                                    [else       (loop (rest j) r)])])))

;; ---------------------------------------------------------------------------------------------------

(define-singleton-type Empty-Tree-Set empty-tree-set)
(define-singleton-type Univ-Tree-Set univ-tree-set)
(struct: (S) Tree-Set-Node ([axis : S] [left : (Tree-Set S)] [right : (Tree-Set S)])
  #:transparent)

(define-type (Tree-Set S) (U Empty-Tree-Set Univ-Tree-Set (Tree-Set-Node S)))

(: tree-set-node (All (S X) ((set S X) S -> (S (Tree-Set S) (Tree-Set S) -> (Tree-Set S)))))
(define (tree-set-node ops full-axis)
  (define empty-axis? (set-empty? ops))
  (λ (A l r)
    (cond [(or (empty-axis? A) (empty-tree-set? l) (empty-tree-set? r))  empty-tree-set]
          [(and (equal? A full-axis) (univ-tree-set? l) (univ-tree-set? r))  univ-tree-set]
          [else  (Tree-Set-Node A l r)])))

(: tree-set-ops (All (S X) ((set S X) S -> (set (Tree-Set S) (Tree-Val X)))))
(define (tree-set-ops ops full-axis)
  (define node (tree-set-node ops full-axis))
  (match-define (set _empty-axis _univ-axis axis-meet axis-join axis-member? axis-singleton) ops)
  
  (: meet ((Tree-Set S) (Tree-Set S) -> (Tree-Set S)))
  (define (meet A1 A2)
    (cond [(or (empty-tree-set? A1) (empty-tree-set? A2))  empty-tree-set]
          [(univ-tree-set? A1)  A2]
          [(univ-tree-set? A2)  A1]
          [else  (match-let ([(Tree-Set-Node A1 L1 R1)  A1]
                             [(Tree-Set-Node A2 L2 R2)  A2])
                   (node (axis-meet A1 A2) (meet L1 L2) (meet R1 R2)))]))
  
  (: join ((Tree-Set S) (Tree-Set S) -> (Tree-Set S)))
  (define (join A1 A2)
    (cond [(or (univ-tree-set? A1) (univ-tree-set? A2))  univ-tree-set]
          [(empty-tree-set? A1)  A2]
          [(empty-tree-set? A2)  A1]
          [else  (match-let ([(Tree-Set-Node A1 L1 R1)  A1]
                             [(Tree-Set-Node A2 L2 R2)  A2])
                   (node (axis-join A1 A2) (join L1 L2) (join R1 R2)))]))
  
  (: member? ((Tree-Set S) (Tree-Val X) -> Boolean))
  (define (member? A a)
    (cond [(empty-tree-set? A)  #f]
          [(univ-tree-set? A)   #t]
          [(any-tree-val? a)  (error 'tree-set-member? "indeterminate value")]
          [else  (match-let ([(Tree-Set-Node A L R)  A]
                             [(tree-val-node a l r)  a])
                   (and (axis-member? A a) (member? L l) (member? R r)))]))
  
  (: singleton ((Tree-Val X) -> (Tree-Set S)))
  (define (singleton a)
    (match a
      [(? any-tree-val?)  univ-tree-set]
      [(tree-val-node a l r)  (node (axis-singleton a) (singleton l) (singleton r))]))
  
  (set empty-tree-set univ-tree-set meet join member? singleton))

(: project (All (S X) ((set S X) S -> (Tree-Index (Tree-Set S) -> S))))
(define (project ops full-axis)
  (define empty-axis (set-empty ops))
  (λ (j A)
    (let loop ([j j] [A A])
      (cond [(empty-tree-set? A)  empty-axis]
            [(univ-tree-set? A)   full-axis]
            [(empty? j)  (Tree-Set-Node-axis A)]
            [(first j)   (loop (rest j) (Tree-Set-Node-left A))]
            [else        (loop (rest j) (Tree-Set-Node-right A))]))))

(: unproject (All (S X) ((set S X) S -> (Tree-Index (Tree-Set S) S -> (Tree-Set S)))))
(define (unproject ops full-axis)
  (define node (tree-set-node ops full-axis))
  (define axis-meet (set-meet ops))
  (λ (j A B)
    (let loop ([j j] [A A])
      (cond [(empty-tree-set? A)  empty-tree-set]
            [(univ-tree-set? A)   (loop j (Tree-Set-Node full-axis univ-tree-set univ-tree-set))]
            [else  (match-let ([(Tree-Set-Node A L R)  A])
                     (cond [(empty? j)  (node (axis-meet A B) L R)]
                           [(first j)   (node A (loop (rest j) L) R)]
                           [else        (node A L (loop (rest j) R))]))]))))
