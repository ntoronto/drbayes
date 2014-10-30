#lang typed/racket/base

(require racket/promise
         racket/list
         racket/match
         "types.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(define-type Tree-Index (Listof (U 0 1)))
(define-type Nonempty-Tree-Index (Pair (U 0 1) Tree-Index))

(: left (Tree-Index -> Tree-Index))
(define (left j) (cons 0 j))

(: right (Tree-Index -> Tree-Index))
(define (right j) (cons 1 j))

;; ===================================================================================================

(struct: (A) Node ([value : (Promise A)] [children : (Children A)]) #:transparent)
(struct: (A) Children ([fst : (Tree A)] [snd : (Tree A)]) #:transparent)
(define-type (Tree A) (U (Node A) (Promise (Node A))))

(: random-tree (All (A) ((-> A) -> (Tree A))))
(define (random-tree rand)
  (delay (Node (delay (rand)) (Children (random-tree rand) (random-tree rand)))))

(: tree-value (All (A) ((Tree A) -> (Promise A))))
(define (tree-value t) (Node-value (maybe-force t)))

(: tree-fst (All (A) ((Tree A) -> (Tree A))))
(define (tree-fst t) (Children-fst (Node-children (maybe-force t))))

(: tree-snd (All (A) ((Tree A) -> (Tree A))))
(define (tree-snd t) (Children-snd (Node-children (maybe-force t))))

(: tree-ref (All (A) ((Tree A) Tree-Index -> A)))
(define (tree-ref t j)
  (let ([t  (maybe-force t)])
    (cond [(empty? j)  (force (Node-value t))]
          [(zero? (first j))  (tree-ref (Children-fst (Node-children t)) (rest j))]
          [else               (tree-ref (Children-snd (Node-children t)) (rest j))])))

(: tree->list (All (A) ((Tree A) -> (Listof A))))
(define (tree->list t)
  (reverse
   (let: loop ([t t] [vs : (Listof A)  null])
     (cond [(or (not (promise? t)) (promise-forced? t))
            (match-define (Node v (Children c1 c2)) (maybe-force t))
            (let ([vs  (loop c1 vs)])
              (if (promise-forced? v)
                  (loop c2 (cons (force v) vs))
                  (loop c2 vs)))]
           [else  vs]))))

;; ===================================================================================================

(struct: Omega Base-Value ([tree : (Tree Flonum)]) #:transparent)

(define omega? Omega?)

(: random-omega (-> Omega))
(define (random-omega) (Omega (random-tree random)))


(: make-omega (Flonum (Children Flonum) -> Omega))
(define (make-omega v c)
  (Omega (Node (delay v) c)))

(: omega-value (Omega -> Flonum))
(define (omega-value t) (maybe-force (tree-value (Omega-tree t))))

(: omega-children (Omega -> (Children Flonum)))
(define (omega-children t) (Node-children (maybe-force (Omega-tree t))))

#|
(: omega-fst (Omega -> Omega))
(define (omega-fst t) (Omega (tree-fst (Omega-tree t))))

(: omega-snd (Omega -> Omega))
(define (omega-snd t) (Omega (tree-snd (Omega-tree t))))
|#

(: make-omega-children (Omega Omega -> (Children Flonum)))
(define (make-omega-children t1 t2)
  (Children (Omega-tree t1) (Omega-tree t2)))

(: omega-children-fst ((Children Flonum) -> Omega))
(define (omega-children-fst t) (Omega (Children-fst t)))

(: omega-children-snd ((Children Flonum) -> Omega))
(define (omega-children-snd t) (Omega (Children-snd t)))


(: omega-ref (Omega Tree-Index -> Flonum))
(define (omega-ref t j) (tree-ref (Omega-tree t) j))

(: omega->list (Omega -> (Listof Flonum)))
(define (omega->list t) (tree->list (Omega-tree t)))

;; ===================================================================================================

(struct: Trace Base-Value ([tree : (Tree Boolean)]) #:transparent)

(define trace? Trace?)

(: random-trace (-> Trace))
(define (random-trace) (Trace (random-tree (λ () ((random) . < . 0.5)))))


(: make-trace (Boolean (Children Boolean) -> Trace))
(define (make-trace v c)
  (Trace (Node (delay v) c)))

(: trace-value (Trace -> Boolean))
(define (trace-value t) (maybe-force (tree-value (Trace-tree t))))

(: trace-children (Trace -> (Children Boolean)))
(define (trace-children t) (Node-children (maybe-force (Trace-tree t))))

#|
(: trace-fst (Trace -> Trace))
(define (trace-fst t) (Trace (tree-fst (Trace-tree t))))

(: trace-snd (Trace -> Trace))
(define (trace-snd t) (Trace (tree-snd (Trace-tree t))))
|#

(: make-trace-children (Trace Trace -> (Children Boolean)))
(define (make-trace-children t1 t2)
  (Children (Trace-tree t1) (Trace-tree t2)))

(: trace-children-fst ((Children Boolean) -> Trace))
(define (trace-children-fst t) (Trace (Children-fst t)))

(: trace-children-snd ((Children Boolean) -> Trace))
(define (trace-children-snd t) (Trace (Children-snd t)))


(: trace-ref (Trace Tree-Index -> Boolean))
(define (trace-ref t j) (tree-ref (Trace-tree t) j))

(: trace->list (Trace -> (Listof Boolean)))
(define (trace->list t) (tree->list (Trace-tree t)))
