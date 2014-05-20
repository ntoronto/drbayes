#lang typed/racket

(require "../types.rkt")

(provide (all-defined-out))

;; Binary indexing scheme

(define-type Tree-Index (Listof (U 0 1)))

(define j0 '())

(: left (Tree-Index -> Tree-Index))
(define (left j) (cons 0 j))

(: right (Tree-Index -> Tree-Index))
(define (right j) (cons 1 j))

;; Branch traces

(define-type Leaf 'leaf)
(define leaf 'leaf)
(define-predicate leaf? Leaf)

(struct: trace-node ([value : Boolean] [fst : Branch-Trace] [snd : Branch-Trace]) #:transparent)
(define-type Branch-Trace (U trace-node 'leaf))

(: π (Tree-Index -> (Branch-Trace -> (U Boolean Bottom))))
(define ((π j) b)
  (cond [(leaf? b)  ⊥]
        [(empty? j)  (trace-node-value b)]
        [(zero? (first j))  ((π (rest j)) (trace-node-fst b))]
        [else               ((π (rest j)) (trace-node-snd b))]))

(: moar-traces (Branch-Trace -> (Listof Branch-Trace)))
(define (moar-traces b)
  (cond [(leaf? b)
         (list (trace-node #t leaf leaf) (trace-node #f leaf leaf))]
        [else
         (match-define (trace-node v fst snd) b)
         (for*/list: : (Listof Branch-Trace) ([b0  (in-list (moar-traces fst))]
                                              [b1  (in-list (moar-traces snd))])
           (trace-node v b0 b1))]))

(: add-traces ((Listof Branch-Trace) -> (Listof Branch-Trace)))
(define (add-traces bs)
  (remove-duplicates (append* (cons bs (map moar-traces bs)))))

(define some-traces
  (list->set (add-traces (add-traces (add-traces (add-traces (list leaf)))))))
