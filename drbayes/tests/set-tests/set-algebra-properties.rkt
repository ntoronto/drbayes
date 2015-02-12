#lang typed/racket

(require typed/rackunit
         "../test-utils.rkt"
         "set-membership-properties.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Set algebra property checks

(: check-complement-trivial (All (T) (-> (T T -> Boolean)
                                         T
                                         T
                                         (T T -> T)
                                         (T T -> T)
                                         (T T -> T)
                                         T
                                         Any)))
(define (check-complement-trivial equal? empty-set universe subtract union intersect A)
  (define Ac (subtract universe A))
  (check-prop (equal? (subtract universe Ac) A)
              (format "~a ~a ~a ~a ~a ~a: double complement failed on ~v"
                      equal? empty-set universe subtract union intersect A))
  (check-prop (equal? (union A Ac) universe)
              (format "~a ~a ~a ~a ~a ~a: complement union failed on ~v"
                      equal? empty-set universe subtract union intersect A))
  (check-prop (equal? (intersect A Ac) empty-set)
              (format "~a ~a ~a ~a ~a ~a: complement intersect failed on ~v"
                      equal? empty-set universe subtract union intersect A)))

(: check-complement-monotone (All (T) ((T T -> Boolean) T (T T -> T) T T -> Any)))
(define (check-complement-monotone subseteq? universe subtract A B)
  (define Ac (subtract universe A))
  (define Bc (subtract universe B))
  (check-prop (implies (subseteq? A B) (subseteq? Bc Ac))
              (format "~a ~a ~a: complement non-monotone on ~v ~v" subseteq? universe subtract A B))
  (check-prop (implies (subseteq? B A) (subseteq? Ac Bc))
              (format "~a ~a ~a: complement non-monotone on ~v ~v" subseteq? universe subtract B A)))

(: check-subtract-trivial (All (T) ((T T -> Boolean) T (T T -> T) T -> Any)))
(define (check-subtract-trivial equal? empty-set subtract A)
  (check-prop (equal? (subtract A A) empty-set)
              (format "~a ~a ~a: subtracting self failed on ~v"
                      equal? empty-set subtract A))
  (check-prop (equal? (subtract empty-set A) empty-set)
              (format "~a ~a ~a: subtracting from empty-set failed on ~v"
                      equal? empty-set subtract A))
  (check-prop (equal? (subtract A empty-set) A)
              (format "~a ~a ~a: subtracting empty-set failed on ~v"
                      equal? empty-set subtract A)))

(: check-subtract-distrib (All (T) (-> (T T -> Boolean)
                                       (T T -> T)
                                       (T T -> T)
                                       (T T -> T)
                                       T T T
                                       Any)))
(define (check-subtract-distrib equal? subtract union intersect A B C)
  (define C\B (subtract C B))
  (define C\A (subtract C A))
  (define B\A (subtract B A))
  (check-prop (equal? (subtract C (intersect A B)) (union C\A C\B))
              (format "~a ~a ~a ~a: \\-∩ failed on ~v ~v ~v" equal? subtract union intersect A B C))
  (check-prop (equal? (subtract C (union A B)) (intersect C\A C\B))
              (format "~a ~a ~a ~a: \\-∪ failed on ~v ~v ~v" equal? subtract union intersect A B C))
  (check-prop (equal? (subtract C B\A) (union (intersect A C) C\B))
              (format "~a ~a ~a ~a: \\-\\ failed on ~v ~v ~v" equal? subtract union intersect A B C))
  (check-prop (equal? (intersect B\A C) (subtract (intersect B C) A))
              (format "~a ~a ~a ~a: \\-∩ failed on ~v ~v ~v" equal? subtract union intersect A B C))
  (check-prop (equal? (intersect B\A C) (intersect B C\A))
              (format "~a ~a ~a ~a: \\-∩ failed on ~v ~v ~v" equal? subtract union intersect A B C))
  (check-prop (equal? (union B\A C) (subtract (union B C) (subtract A C)))
              (format "~a ~a ~a ~a: \\-∪ failed on ~v ~v ~v" equal? subtract union intersect A B C)))

(: check-subtract (All (T X) ((T X -> Boolean) (T T -> T) T X T X -> Any)))
(define (check-subtract member? subtract A x B y)
  (define C (subtract A B))
  (for: ([x  (in-list (list x y))])
    (check-prop (implies (and (member? A x) (member? B x)) (not (member? C x)))
                (format "~a ~a: subtract membership failed on ~v ~v ~v" member? subtract A B x))
    (check-prop (implies (and (member? A x) (not (member? B x))) (member? C x))
                (format "~a ~a: subtract membership failed on ~v ~v ~v" member? subtract A B x))
    (check-prop (implies (not (member? A x)) (not (member? C x)))
                (format "~a ~a: subtract membership failed on ~v ~v ~v" member? subtract A B x))))

(: check-union (All (T X) (-> (T X -> Boolean) (T T -> T) T X T X Any)))
(define (check-union member? union A x B y)
  (define C (union A B))
  (for: ([x  (in-list (list x y))])
    (check-prop (implies (or (member? A x) (member? B x)) (member? C x))
                (format "~a ~a: union membership failed on ~v ~v ~v" member? union A B x))
    (check-prop (implies (not (or (member? A x) (member? B x))) (not (member? C x)))
                (format "~a ~a: union membership failed on ~v ~v ~v" member? union A B x))))

(: check-intersect (All (T X) ((T X -> Boolean) (T T -> T) T X T X -> Any)))
(define (check-intersect member? intersect A x B y)
  (define C (intersect A B))
  (for: ([x  (in-list (list x y))])
    (check-prop (implies (and (member? A x) (member? B x)) (member? C x))
                (format "~a ~a: intersect membership failed on ~v ~v ~v" member? intersect A B x))
    (check-prop (implies (not (and (member? A x) (member? B x))) (not (member? C x)))
                (format "~a ~a: intersect membership failed on ~v ~v ~v" member? intersect A B x))))

;; ===================================================================================================
;; All set algebra checks

(: check-set-algebra (All (T X) ((T T -> Boolean)
                                 (T X -> Boolean)
                                 (T T -> Boolean)
                                 T
                                 T
                                 (T T -> T)
                                 (T T -> T)
                                 (T T -> T)
                                 (-> T)
                                 (T -> X) -> Any)))
(define (check-set-algebra equal? member? subseteq? empty-set universe subtract union intersect
                           random-set random-member)
  (define A (random-set))
  (define B (random-set))
  (define C (random-set))
  
  ;(printf "A = ~v~n" A)
  ;(printf "B = ~v~n" B)
  
  (check-subtract-trivial equal? empty-set subtract A)
  (check-subtract-distrib equal? subtract union intersect A B C)
  
  (check-complement-trivial equal? empty-set universe subtract union intersect A)
  (check-complement-monotone subseteq? universe subtract A B)
  
  (when (and (not (equal? A empty-set)) (not (equal? B empty-set)))
    (define x (random-member A))
    (define y (random-member B))
    
    ;(printf "x = ~v~n" x)
    ;(printf "y = ~v~n" y)
    
    (check-member member? A x)
    (check-member member? B y)
    
    (check-subseteq member? subseteq? A x B y)
    
    (check-subtract member? subtract A x B y)
    (check-union member? union A x B y)
    (check-intersect member? intersect A x B y)))
