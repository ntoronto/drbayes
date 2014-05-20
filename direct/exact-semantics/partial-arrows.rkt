#lang typed/racket

(require "../types.rkt"
         "set-ops.rkt"
         "branch-trace.rkt"
         "bot-arrow.rkt"
         "map-arrow.rkt"
         "pre-arrow.rkt"
         )

(provide (except-out (all-defined-out) define-transformed-arrow))

(define-syntax-rule (define-transformed-arrow
                      In-Arrow arr/a >>>/a &&&/a ifte/a lazy/a agrees/a π/a
                      Out-Arrow eta/a* arr/a* >>>/a* &&&/a* ifte/a* ifte-conv/a* lazy/a*)
  (begin
    (define-type (Out-Arrow X Y) (Tree-Index -> (In-Arrow (Pair Branch-Trace X) Y)))
    
    (: fst/a (All (X Y) (In-Arrow (Pair X Y) X)))
    (define (fst/a xy)
      (((inst arr/a (Pair X Y) X) car) xy))
    
    (: snd/a (All (X Y) (In-Arrow (Pair X Y) Y)))
    (define (snd/a xy)
      (((inst arr/a (Pair X Y) Y) cdr) xy))
    
    (: eta/a* (All (X Y) ((In-Arrow X Y) -> (Out-Arrow X Y))))
    (define ((eta/a* f) j)
      ((inst snd/a Branch-Trace X) . >>>/a . f))
    
    (: arr/a* (All (X Y) ((X -> Y) -> (Out-Arrow X Y))))
    (define (arr/a* f)
      (eta/a* ((inst arr/a X Y) f)))
    
    (: >>>/a* (All (X Y Z) ((Out-Arrow X Y) (Out-Arrow Y Z) -> (Out-Arrow X Z))))
    (define ((f1 . >>>/a* . f2) j)
      (((inst fst/a Branch-Trace X) . &&&/a . (f1 (left j))) . >>>/a . (f2 (right j))))
    
    (: &&&/a* (All (X Y Z) ((Out-Arrow X Y) (Out-Arrow X Z) -> (Out-Arrow X (Pair Y Z)))))
    (define ((f1 . &&&/a* . f2) j)
      ((f1 (left j)) . &&&/a . (f2 (right j))))
    
    (: ifte/a* (All (X Y) ((Out-Arrow X Boolean) (Out-Arrow X Y) (Out-Arrow X Y) -> (Out-Arrow X Y))))
    (define ((ifte/a* f1 f2 f3) j)
      (ifte/a (f1 (left j))
              (f2 (left (right j)))
              (f3 (right (right j)))))
    
    (: lazy/a* (All (X Y) ((-> (Out-Arrow X Y)) -> (Out-Arrow X Y))))
    (define ((lazy/a* f) j)
      (lazy/a (λ () ((f) j))))
    
    (: branch/a* (All (X) (Out-Arrow X Boolean)))
    (define (branch/a* j)
      ((inst fst/a Branch-Trace X) . >>>/a . (π/a j)))
    
    (: ifte-conv/a* (All (X Y) ((Out-Arrow X Boolean) (Out-Arrow X Y) (Out-Arrow X Y)
                                                      -> (Out-Arrow X Y))))
    (define ((ifte-conv/a* f1 f2 f3) j)
      (ifte/a (((f1 (left j)) . &&&/a . ((inst branch/a* X) j)) . >>>/a . agrees/a)
              (f2 (left (right j)))
              (f3 (right (right j)))))
    
    ))

;; ===================================================================================================
;; Partial bottom arrow

(define-transformed-arrow
  Bot-Arrow arr/bot >>>/bot &&&/bot ifte/bot lazy/bot agrees/bot π/bot
  Bot*-Arrow eta/bot* arr/bot* >>>/bot* &&&/bot* ifte/bot* ifte-conv/bot* lazy/bot*)

(: ap/bot* (All (X Y) ((Bot*-Arrow X Y) X -> (Maybe Y))))
(define (ap/bot* f x)
  (define B (set-image (f j0) (set-product some-traces (set x))))
  (define C ((inst set-filter-out (just Y) Bottom) ⊥? B))
  (if (set-empty? C) ⊥ (set-take C)))

(: id/bot* (All (X) (Bot*-Arrow X X)))
(define (id/bot* x)
  (((inst arr/bot* X X) (λ (x) x)) x))

(: const/bot* (All (X Y) (Y -> (Bot*-Arrow X Y))))
(define (const/bot* y)
  ((inst arr/bot* X Y) (λ (x) y)))

(: fst/bot* (All (X Y) (Bot*-Arrow (Pair X Y) X)))
(define (fst/bot* xy)
  (((inst arr/bot* (Pair X Y) X) car) xy))

(: snd/bot* (All (X Y) (Bot*-Arrow (Pair X Y) Y)))
(define (snd/bot* xy)
  (((inst arr/bot* (Pair X Y) Y) cdr) xy))

;; ===================================================================================================
;; Partial mapping arrow

(define-transformed-arrow
  Map-Arrow arr/map >>>/map &&&/map ifte/map lazy/map agrees/map π/map
  Map*-Arrow eta/map* arr/map* >>>/map* &&&/map* ifte/map* ifte-conv/map* lazy/map*)

(: lift/map* (All (X Y) ((Bot*-Arrow X Y) -> (Map*-Arrow X Y))))
(define ((lift/map* f) j)
  (lift/map (f j)))

(: id/map* (All (X) (Map*-Arrow X X)))
(define (id/map* x)
  (((inst arr/map* X X) (λ (x) x)) x))

(: const/map* (All (X Y) (Y -> (Map*-Arrow X Y))))
(define (const/map* y)
  ((inst arr/map* X Y) (λ (x) y)))

(: fst/map* (All (X Y) (Map*-Arrow (Pair X Y) X)))
(define (fst/map* xy)
  (((inst arr/map* (Pair X Y) X) car) xy))

(: snd/map* (All (X Y) (Map*-Arrow (Pair X Y) Y)))
(define (snd/map* xy)
  (((inst arr/map* (Pair X Y) Y) cdr) xy))

;; ===================================================================================================
;; Partial preimage arrow

(define-transformed-arrow
  Pre-Arrow arr/pre >>>/pre &&&/pre ifte/pre lazy/pre agrees/pre π/pre
  Pre*-Arrow eta/pre* arr/pre* >>>/pre* &&&/pre* ifte/pre* ifte-conv/pre* lazy/pre*)

(: lift/pre* (All (X Y) ((Map*-Arrow X Y) -> (Pre*-Arrow X Y))))
(define ((lift/pre* f) j)
  (lift/pre (f j)))

(: id/pre* (All (X) (Pre*-Arrow X X)))
(define (id/pre* x)
  (((inst arr/pre* X X) (λ (x) x)) x))

(: const/pre* (All (X Y) (Y -> (Pre*-Arrow X Y))))
(define (const/pre* y)
  ((inst arr/pre* X Y) (λ (x) y)))

(: fst/pre* (All (X Y) (Pre*-Arrow (Pair X Y) X)))
(define (fst/pre* xy)
  (((inst arr/pre* (Pair X Y) X) car) xy))

(: snd/pre* (All (X Y) (Pre*-Arrow (Pair X Y) Y)))
(define (snd/pre* xy)
  (((inst arr/pre* (Pair X Y) Y) cdr) xy))
