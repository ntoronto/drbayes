#lang typed/racket

(provide (all-defined-out))

;; ===================================================================================================
;; Function arrow

(define-type (Fun-Arrow X Y) (X -> Y))

(: arr (All (X Y) ((X -> Y) -> (Fun-Arrow X Y))))
(define (arr f) f)

(: >>> (All (X Y Z) ((Fun-Arrow X Y) (Fun-Arrow Y Z) -> (Fun-Arrow X Z))))
(define ((f1 . >>> . f2) x) (f2 (f1 x)))

(: &&& (All (X Y Z) ((Fun-Arrow X Y) (Fun-Arrow X Z) -> (Fun-Arrow X (Pair Y Z)))))
(define ((f1 . &&& . f2) x) (cons (f1 x) (f2 x)))

(: lazy (All (X Y) ((-> (Fun-Arrow X Y)) -> (Fun-Arrow X Y))))
(define ((lazy f) x) ((f) x))

(: ifte (All (X Y) ((Fun-Arrow X Boolean) (Fun-Arrow X Y) (Fun-Arrow X Y) -> (Fun-Arrow X Y))))
(define ((ifte c t f) x)
  (if (c x) (t x) (f x)))

;; ---------------------------------------------------------------------------------------------------
;; Function arrow instances

(: id (All (X) (Fun-Arrow X X)))
(define (id x) x)

(: const (All (X Y) (Y -> (Fun-Arrow X Y))))
(define ((const y) x) y)

(: fst (All (X Y) (Fun-Arrow (Pair X Y) X)))
(define (fst xy) (car xy))

(: snd (All (X Y) (Fun-Arrow (Pair X Y) Y)))
(define (snd xy) (cdr xy))
