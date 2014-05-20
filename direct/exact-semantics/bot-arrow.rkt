#lang typed/racket

(require "../types.rkt"
         "branch-trace.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Bottom arrow

(define-type (Bot-Arrow X Y) (X -> (Maybe Y)))

(: arr/bot (All (X Y) ((X -> Y) -> (Bot-Arrow X Y))))
(define ((arr/bot f) x) (just (f x)))

(: >>>/bot (All (X Y Z) ((Bot-Arrow X Y) (Bot-Arrow Y Z) -> (Bot-Arrow X Z))))
(define ((f1 . >>>/bot . f2) x)
  (let ([y  (f1 x)])
    (if (⊥? y) y (f2 (just-value y)))))

(: &&&/bot (All (X Y Z) ((Bot-Arrow X Y) (Bot-Arrow X Z) -> (Bot-Arrow X (Pair Y Z)))))
(define ((f1 . &&&/bot . f2) x)
  (let ([y  (f1 x)]
        [z  (f2 x)])
    (if (or (⊥? y) (⊥? z)) ⊥ (just (cons (just-value y) (just-value z))))))

(: ifte/bot (All (X Y) ((Bot-Arrow X Boolean) (Bot-Arrow X Y) (Bot-Arrow X Y) -> (Bot-Arrow X Y))))
(define ((ifte/bot c t f) x)
  (define b (c x))
  (cond [(⊥? b)  b]
        [else  (let ([b  (just-value b)])
                 (if b (t x) (f x)))]))

(: lazy/bot (All (X Y) ((-> (Bot-Arrow X Y)) -> (Bot-Arrow X Y))))
(define ((lazy/bot f) x) ((f) x))

;; ---------------------------------------------------------------------------------------------------
;; Bottom arrow lifts

(: id/bot (All (X) (Bot-Arrow X X)))
(define (id/bot x)
  (((inst arr/bot X X) (λ (x) x)) x))

(: const/bot (All (X Y) (Y -> (Bot-Arrow X Y))))
(define (const/bot y)
  ((inst arr/bot X Y) (λ (x) y)))

(: fst/bot (All (X Y) (Bot-Arrow (Pair X Y) X)))
(define (fst/bot xy)
  (((inst arr/bot (Pair X Y) X) car) xy))

(: snd/bot (All (X Y) (Bot-Arrow (Pair X Y) Y)))
(define (snd/bot xy)
  (((inst arr/bot (Pair X Y) Y) cdr) xy))

(: agrees/bot (Bot-Arrow (Pair Boolean Boolean) Boolean))
(define (agrees/bot xy)
  (if (equal? (car xy) (cdr xy)) (just (car xy)) ⊥))

(: π/bot (Tree-Index -> (Bot-Arrow Branch-Trace Boolean)))
(define ((π/bot j) b)
  (define x ((π j) b))
  (if (⊥? x) x (just x)))

#|
(: halt-on-true/bot (Bot-Arrow Boolean Boolean))
(define halt-on-true/bot
  (ifte/bot (inst id/bot Boolean)
            (lazy/bot (λ () ((inst const/bot Boolean Boolean) #t)))
            (lazy/bot (λ () halt-on-true/bot))))

((&&&/bot (λ (a) ⊥) halt-on-true/bot) true)
((&&&/bot (λ (a) ⊥) halt-on-true/bot) false)
|#
