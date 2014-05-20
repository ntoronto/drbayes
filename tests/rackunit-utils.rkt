#lang typed/racket

(require typed/rackunit)

(provide (all-defined-out))

(define-syntax-rule (implies a b) (or (not a) b))

(: flip (All (A B C) ((A B -> C) -> (B A -> C))))
(define ((flip f) x y) (f y x))

;; Using this is about 1000x faster than using `check-true' directly, mostly because it doesn't have
;; to construct the message unless there's a failure
(define-syntax-rule (check-prop expr msg)
  (if expr (void) (check-true expr msg)))

;; ===================================================================================================
;; Bounded lattice property checks

;; Partial order properties

(: check-reflexive (All (T) ((T T -> Boolean) T -> Any)))
(define (check-reflexive lte? A)
  (check-prop (lte? A A)
              (format "~a: reflexivity failed on ~v" lte? A)))

(: check-antisymmetric (All (T) ((T T -> Boolean) (T T -> Boolean) T T -> Any)))
(define (check-antisymmetric equal? lte? A B)
  (check-prop (implies (and (lte? A B) (lte? B A))
                       (equal? A B))
              (format "~a ~a: antisymmetry failed on ~v ~v" equal? lte? A B)))

(: check-transitive (All (T) ((T T -> Boolean) T T T -> Any)))
(define (check-transitive lte? A B C)
  (check-prop (implies (and (lte? A B) (lte? B C))
                       (lte? A C))
              (format "~a: transitivity failed on ~v ~v ~v" lte? A B C)))

;; Lattice operator properties

(: check-identity (All (T) ((T T -> Boolean) (T T -> T) T T -> Any)))
(define (check-identity equal? op id A)
  (check-prop (equal? (op id A) A)
              (format "~a ~a ~a: identity failed on ~v" equal? op id A)))

(: check-commutative (All (T) ((T T -> Boolean) (T T -> T) T T -> Any)))
(define (check-commutative equal? op A B)
  (check-prop (equal? (op A B) (op B A))
              (format "~a ~a: commutativity failed on ~v ~v" equal? op A B)))

(: check-absorption (All (T) ((T T -> Boolean) (T T -> T) (T T -> T) T T -> Any)))
(define (check-absorption equal? op1 op2 A B)
  (check-prop (equal? (op1 A (op2 A B)) A)
              (format "~a ~a ~a: absorption failed on ~v ~v" equal? op1 op2 A B)))

(: check-associative (All (T) ((T T -> Boolean) (T T -> T) T T T -> Any)))
(define (check-associative equal? op A B C)
  (check-prop (equal? (op (op A B) C) (op A (op B C)))
              (format "~a ~a: associativity failed on ~v ~v ~v" equal? op A B C)))

;; Other properties (those that involve both the partial order and the operations)

(: check-nondecreasing (All (T) ((T T -> Boolean) (T T -> T) T T -> Any)))
(define (check-nondecreasing lte? op A B)
  (define C (op A B))
  (check-prop (and (lte? A C) (lte? B C))
              (format "~a ~a: nondecreasing failed on ~v ~v" lte? op A B)))

(: check-order-equiv (All (T) ((T T -> Boolean) (T T -> Boolean) (T T -> T) (T T -> T) T T -> Any)))
(define (check-order-equiv equal? lte? join meet A B)
  (check-prop (eq? (or (equal? A (meet A B))
                       (equal? B (join A B)))
                   (lte? A B))
              (format "~a ~a ~a ~a: equivalent order definition failed on ~v ~v"
                      equal? lte? join meet A B)))

(: check-monotone (All (T) ((T T -> Boolean) (T T -> T) T T T T -> Any)))
(define (check-monotone lte? op A1 A2 B1 B2)
  (check-prop (implies (and (lte? A1 A2) (lte? B1 B2))
                       (lte? (op A1 B1) (op A2 B2)))
              (format "~a ~a: monotonicity failed on ~v ~v ~v ~v" lte? op A1 A2 B1 B2)))

;; All bounded lattice properties

(: check-bounded-lattice
   (All (T) ((T T -> Boolean) (T T -> Boolean) (T T -> T) (T T -> T) T T (-> T) -> Any)))
(define (check-bounded-lattice equal? lte? join meet bot top random-set)
  (define A (random-set))
  (define B (random-set))
  (define C (random-set))
  (define D (random-set))
  
  ;(printf "A = ~v~n" A)
  ;(printf "B = ~v~n" B)
  ;(printf "C = ~v~n" C)
  ;(printf "D = ~v~n~n" D)
  
  ;; Partial order properties
  
  (check-reflexive lte? A)
  (check-antisymmetric equal? lte? A B)
  (check-transitive lte? A B C)
  
  ;; Lattice operator properties
  
  (check-identity equal? join bot A)
  (check-identity equal? meet top A)
  (check-commutative equal? join A B)
  (check-commutative equal? meet A B)
  (check-associative equal? join A B C)
  (check-associative equal? meet A B C)
  (check-absorption equal? join meet A B)
  (check-absorption equal? meet join A B)
  
  ;; Other properties
  
  (check-nondecreasing lte? join A B)
  (check-nondecreasing (flip lte?) meet A B)
  (check-order-equiv equal? lte? join meet A B)
  (check-monotone lte? join A B C D)
  (check-monotone lte? meet A B C D))

;; ===================================================================================================
;; Checks for lattices with membership

(: check-member (All (T X) ((T X -> Boolean) T X -> Any)))
(define (check-member member? A x)
  (check-prop (member? A x) (format "~a: membership failed on ~v ~v" member? A x)))

(: check-subseteq (All (T X) ((T X -> Boolean) (T T -> Boolean) T X T X -> Any)))
(define (check-subseteq member? subseteq? A x B y)
  (when (subseteq? A B)
    (for: ([x  (in-list (list x y))])
      (check-prop (implies (member? A x) (member? B x))
                  (format "~a ~a: subseteq membership failed on ~v ~v ~v" member? subseteq? A B x))
      (check-prop (implies (not (member? B x)) (not (member? A x)))
                  (format "~a ~a: subseteq non-membership failed on ~v ~v ~v"
                          member? subseteq? A B x)))))

(: check-join (All (T X) ((T X -> Boolean) (T T -> T) T X T X -> Any)))
(define (check-join member? join A x B y)
  (define C (join A B))
  (for: ([x  (in-list (list x y))])
    (check-prop (implies (or (member? A x) (member? B x)) (member? C x))
                (format "~a ~a: join membership failed on ~v ~v ~v" member? join A B x))))

(: check-meet (All (T X) ((T X -> Boolean) (T T -> T) T X T X -> Any)))
(define (check-meet member? meet A x B y)
  (define C (meet A B))
  (for: ([x  (in-list (list x y))])
    (check-prop (implies (and (member? A x) (member? B x)) (member? C x))
                (format "~a ~a: meet membership failed on ~v ~v ~v" member? meet A B x))))

;; All membership lattice properties

(: check-membership-lattice (All (T X) ((T -> Boolean)
                                        (T X -> Boolean)
                                        (T T -> Boolean)
                                        (T T -> T)
                                        (T T -> T)
                                        (-> T)
                                        (T -> X) -> Any)))
(define (check-membership-lattice empty? member? subseteq? join meet random-set random-member)
  (define A (random-set))
  (define B (random-set))
  
  ;(printf "A = ~v~n" A)
  ;(printf "B = ~v~n" B)

   (when (and (not (empty? A)) (not (empty? B)))
     (define x (random-member A))
     (define y (random-member B))
     
     ;(printf "x = ~v~n" x)
     ;(printf "y = ~v~n" y)
     
     (check-member member? A x)
     (check-member member? B y)
     
     (check-subseteq member? subseteq? A x B y)
     
     (check-join member? join A x B y)
     (check-meet member? meet A x B y)))

;; ===================================================================================================
;; Set algebra checks

(: check-complement-trivial
   (All (T) ((T T -> Boolean) T T (T T -> T) (T T -> T) (T T -> T) T -> Any)))
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

(: check-subtract-distrib (All (T) ((T T -> Boolean) (T T -> T) (T T -> T) (T T -> T) T T T -> Any)))
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

(: check-union (All (T X) ((T X -> Boolean) (T T -> T) T X T X -> Any)))
(define (check-union member? union A x B y)
  (define C (union A B))
  (for: ([x  (in-list (list x y))])
    (check-prop (implies (or (member? A x) (member? B x)) (member? C x))
                (format "~a ~a: union membership failed on ~v ~v ~v" member? union A B x))
    (check-prop (implies (and (not (member? A x)) (not (member? B x))) (not (member? C x)))
                (format "~a ~a: union membership failed on ~v ~v ~v" member? union A B x))))

(: check-intersect (All (T X) ((T X -> Boolean) (T T -> T) T X T X -> Any)))
(define (check-intersect member? intersect A x B y)
  (define C (intersect A B))
  (for: ([x  (in-list (list x y))])
    (check-prop (implies (and (member? A x) (member? B x)) (member? C x))
                (format "~a ~a: intersect membership failed on ~v ~v ~v" member? intersect A B x))
    (check-prop (implies (or (not (member? A x)) (not (member? B x))) (not (member? C x)))
                (format "~a ~a: intersect membership failed on ~v ~v ~v" member? intersect A B x))))

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
