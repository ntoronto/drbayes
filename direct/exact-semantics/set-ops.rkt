#lang typed/racket

(provide (all-defined-out))

;; ===================================================================================================
;; Additional common set ops

(: set-disjoint? (All (X) ((Setof X) (Setof X) -> Boolean)))
(define (set-disjoint? A B)
  (set-empty? (set-intersect A B)))

(: set-disjoint-union (All (X) ((Setof X) (Setof X) -> (Setof X))))
(define (set-disjoint-union A B)
  (cond [(set-disjoint? A B)  (set-union A B)]
        [else  (error 'set-disjoint-union "expected disjoint sets; given ~e and ~e" A B)]))

(: set-image (All (X Y) ((X -> Y) (Setof X) -> (Setof Y))))
(define (set-image f A)
  (list->set (set-map A f)))

(: set-filter (All (X) ((X -> Boolean) (Setof X) -> (Setof X))))
(define (set-filter f A)
  (list->set (filter f (set->list A))))

(: set-filter-out (All (X Y) (((U X Y) -> Boolean : Y) (Setof (U X Y)) -> (Setof X))))
(define (set-filter-out pred? A)
  (let: loop ([lst  (set->list A)] [acc : (Listof X)  empty])
    (cond [(empty? lst)  (list->set acc)]
          [(pred? (first lst))  (loop (rest lst) acc)]
          [else  (loop (rest lst) (cons (first lst) acc))])))

(: set-preimage (All (X Y) ((X -> Y) (Setof X) (Setof Y) -> (Setof X))))
(define (set-preimage f A B)
  (set-filter (λ: ([x : X]) (set-member? B (f x))) A))

(: set-product (All (X Y) ((Setof X) (Setof Y) -> (Setof (Pair X Y)))))
(define (set-product A B)
  (list->set
   (for*/list: : (Listof (Pair X Y)) ([x  (in-list (set->list A))]
                                      [y  (in-list (set->list B))])
     (cons x y))))

(: set-fst (All (X Y) ((Setof (Pair X Y)) -> (Setof X))))
(define (set-fst AB)
  (set-image (inst car X Y) AB))

(: set-snd (All (X Y) ((Setof (Pair X Y)) -> (Setof Y))))
(define (set-snd AB)
  (set-image (inst cdr X Y) AB))

(: set-power (All (X) ((Setof X) -> (Setof (Setof X)))))
(define (set-power A)
  (let* ([lst  (set->list A)]
         [n    (length lst)])
    (list->set
     (for/list: : (Listof (Setof X)) ([i  (in-range 0 (expt 2 n))])
       (list->set
        (apply append
               (for/list: : (Listof (Listof X)) ([b  (in-range n)])
                 (if (zero? (bitwise-and i (expt 2 (max 0 b))))
                     (list)
                     (list (list-ref lst b))))))))))

(: set-take (All (X) ((Setof X) -> X)))
(define (set-take A)
  (define x (set-first A))
  (define B (set-rest A))
  (cond [(set-empty? B)  x]
        [else  (raise-argument-error 'set-take "singleton Set" A)]))

;; ===================================================================================================

(: set-algebra (All (X) ((Setof X) -> (Setof (Setof X)))))
(define (set-algebra A) (set-power A))

(: set-union-pairs (All (X) ((Setof (Setof X)) -> (Setof (Setof X)))))
(define (set-union-pairs A)
  (set-bind A (λ: ([B : (Setof X)])
                (set-bind A (λ: ([C : (Setof X)])
                              (set (set-union B C)))))))

(: set-intersect-pairs (All (X) ((Setof (Setof X)) -> (Setof (Setof X)))))
(define (set-intersect-pairs A)
  (set-bind A (λ: ([B : (Setof X)])
                (set-bind A (λ: ([C : (Setof X)])
                              (set (set-intersect B C)))))))

(: set-complement-pairs (All (X) ((Setof (Setof X)) -> (Setof (Setof X)))))
(define (set-complement-pairs A)
  (define U (set-union* A))
  (set-bind A (λ: ([A : (Setof X)])
                (set (set-subtract U A)))))

(: set-algebra-close (All (X) ((Setof (Setof X)) -> (Setof (Setof X)))))
(define (set-algebra-close A)
  (let* ([B  (set-union A (set-union-pairs A))]
         [B  (set-union B (set-intersect-pairs A))]
         [B  (set-union B (set-complement-pairs A))])
    (if (equal? A B) B (set-algebra-close B))))

(: set-algebra-rectangles (All (X Y) ((Setof (Setof X)) (Setof (Setof Y))
                                                        -> (Setof (Setof (Pair X Y))))))
(define (set-algebra-rectangles As Bs)
  (set-bind As (λ: ([A : (Setof X)])
                 (set-bind Bs (λ: ([B : (Setof Y)])
                                (set (set-product A B)))))))

(: set-product-algebra (All (X Y) ((Setof (Setof X)) (Setof (Setof Y))
                                                     -> (Setof (Setof (Pair X Y))))))
(define (set-product-algebra As Bs)
  (set-algebra-close (set-algebra-rectangles As Bs)))

(define-type Binary-Set-Op (All (Z) ((Setof Z) (Setof Z) -> (Setof Z))))

(: set-intersect2 Binary-Set-Op)
(define (set-intersect2 A B) (set-intersect A B))

(: set-complement Binary-Set-Op)
(define (set-complement A B) (set-subtract A B))

;; ===================================================================================================
;; Set monad

(: set-lift (All (X Y) ((X -> Y) -> ((Setof X) -> (Setof Y)))))
(define ((set-lift f) A) (set-image f A))

(: set-union* (All (X) ((Setof (Setof X)) -> (Setof X))))
(define (set-union* As)
  (let ([As  (set->list As)])
    (cond [(empty? As)  (set)]
          [else  (apply set-union (first As) (rest As))])))

(: set-bind (All (X Y) ((Setof X) (X -> (Setof Y)) -> (Setof Y))))
(define (set-bind A f)
  (set-union* ((set-lift f) A)))
