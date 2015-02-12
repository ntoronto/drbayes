#lang typed/racket

(require typed/rackunit
         "../test-utils.rkt")

(provide (all-defined-out))

(: check-absorption-exactness? (Parameterof Boolean))
(: check-exactness-commutativity? (Parameterof Boolean))

(define check-absorption-exactness? (make-parameter #t))
(define check-exactness-commutativity? (make-parameter #t))

;; ===================================================================================================
;; Bounded lattice property checks

;; ---------------------------------------------------------------------------------------------------
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

;; ---------------------------------------------------------------------------------------------------
;; Lattice operator properties

(: check-identity (All (T) ((T T -> Boolean) (T T -> (Values T Boolean)) T T -> Any)))
(define (check-identity equal? op id A)
  (define-values (B B-exact?) (op id A))
  (check-prop B-exact?
              (format "~a ~a ~a: identity failed to be exact on ~v" equal? op id A))
  (check-prop (equal? B A)
              (format "~a ~a ~a: identity failed on ~v" equal? op id A)))

(: check-commutative (All (T) ((T T -> Boolean) (T T -> (Values T Boolean)) T T -> Any)))
(define (check-commutative equal? op A B)
  (define-values (C1 C1-exact?) (op A B))
  (define-values (C2 C2-exact?) (op B A))
  (when (check-exactness-commutativity?)
    (check-prop (eq? C1-exact? C2-exact?)
                (format "~a ~a: exactness commutativity failed on ~v ~v" equal? op A B)))
  (check-prop (equal? C1 C2)
              (format "~a ~a: commutativity failed on ~v ~v" equal? op A B)))

(: check-absorption (All (T) (-> (T T -> Boolean)
                                 (T T -> (Values T Boolean))
                                 (T T -> (Values T Boolean))
                                 T T
                                 Any)))
(define (check-absorption equal? op1 op2 A B)
  (define-values (C C-exact?) (op2 A B))
  (define-values (D D-exact?) (op1 A C))
  (when (check-absorption-exactness?)
    (check-prop D-exact?
                (format "~a ~a ~a: absorption exactness failed on ~v ~v" equal? op1 op2 A B)))
  (check-prop (equal? D A)
              (format "~a ~a ~a: absorption failed on ~v ~v" equal? op1 op2 A B)))

(: check-associative (All (T) (-> (T T -> Boolean)
                                  (T T -> (Values T Boolean))
                                  T T T
                                  Any)))
(define (check-associative equal? op A B C)
  (define-values (AB AB-exact?) (op A B))
  (define-values (BC BC-exact?) (op B C))
  (define-values (ABC1 ABC1-exact?) (op AB C))
  (define-values (ABC2 ABC2-exact?) (op A BC))
  (check-prop (equal? ABC1 ABC2)
              (format "~a ~a: associativity failed on ~v ~v ~v" equal? op A B C)))

;; ---------------------------------------------------------------------------------------------------
;; Other properties (those that involve both the partial order and the operations)

(: check-nondecreasing (All (T) ((T T -> Boolean) (T T -> (Values T Boolean)) T T -> Any)))
(define (check-nondecreasing lte? op A B)
  (define-values (C C-exact?) (op A B))
  (check-prop (and (lte? A C) (lte? B C))
              (format "~a ~a: nondecreasing failed on ~v ~v" lte? op A B)))

(: check-order-equiv (All (T) (-> (T T -> Boolean)
                                  (T T -> Boolean)
                                  (T T -> (Values T Boolean))
                                  (T T -> (Values T Boolean))
                                  T T
                                  Any)))
(define (check-order-equiv equal? lte? join meet A B)
  (define-values (meet-AB meet-AB-exact?) (meet A B))
  (define-values (join-AB join-AB-exact?) (join A B))
  (check-prop (eq? (or (equal? A meet-AB)
                       (equal? B join-AB))
                   (lte? A B))
              (format "~a ~a ~a ~a: equivalent order definition failed on ~v ~v"
                      equal? lte? join meet A B)))

(: check-monotone (All (T) ((T T -> Boolean) (T T -> (Values T Boolean)) T T T T -> Any)))
(define (check-monotone lte? op A1 A2 B1 B2)
  (define-values (AB1 AB1-exact?) (op A1 B1))
  (define-values (AB2 AB2-exact?) (op A2 B2))
  (check-prop (implies (and (lte? A1 A2) (lte? B1 B2))
                       (lte? AB1 AB2))
              (format "~a ~a: monotonicity failed on ~v ~v ~v ~v" lte? op A1 A2 B1 B2)))

;; ===================================================================================================
;; All bounded lattice checks

(: check-bounded-lattice (All (T) (-> (T T -> Boolean)
                                      (T T -> Boolean)
                                      (T T -> (Values T Boolean))
                                      (T T -> (Values T Boolean))
                                      T
                                      T
                                      (-> T)
                                      Any)))
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
