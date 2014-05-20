#lang typed/racket

(require typed/rackunit
         "../types.rkt"
         "set-ops.rkt"
         "branch-trace.rkt"
         "bot-arrow.rkt"
         "map-arrow.rkt"
         "pre-arrow.rkt"
         "partial-arrows.rkt"
         "partial-arrows-plus.rkt"
         "mapping.rkt"
         "preimage-mapping.rkt"
         )

(: test-lift/map (All (X Y) ((Bot-Arrow X Y) (Map-Arrow X Y) (Setof X) -> Any)))
(define (test-lift/map f g A)
  (define g* ((inst lift/map X Y) f))
  (check-true (equal? (g* A) (g A))))

(: test-lift/pre (All (X Y) ((Map-Arrow X Y) (Pre-Arrow X Y) (Setof X) -> Any)))
(define (test-lift/pre g h A)
  (define h* ((inst lift/pre X Y) g))
  (check-true (pmapping-equal? (h* A) (h A))))

(: test-lift/map* (All (X Y) ((Bot*-Arrow X Y) (Map*-Arrow X Y) (Setof (Pair Branch-Trace X))
                                               -> Any)))
(define (test-lift/map* f g A)
  (define g* ((inst lift/map* X Y) f))
  (check-true (equal? ((g* j0) A) ((g j0) A))))

(: test-lift/map+ (All (X Y) ((Bot+-Arrow X Y) (Map+-Arrow X Y) (Setof (Pair Branch-Trace X))
                                               -> Any)))
(define (test-lift/map+ f g A)
  (define g+ ((inst lift/map+ X Y) f))
  (check-true (equal? (g+ A) (g A))))

(: test-lift/pre* (All (X Y) ((Map*-Arrow X Y) (Pre*-Arrow X Y) (Setof (Pair Branch-Trace X))
                                               -> Any)))
(define (test-lift/pre* g h A)
  (define h* ((inst lift/pre* X Y) g))
  (check-true (pmapping-equal? ((h* j0) A) ((h j0) A))))

(: test-lift/pre+ (All (X Y) ((Map+-Arrow X Y) (Pre+-Arrow X Y) (Setof (Pair Branch-Trace X))
                                               -> Any)))
(define (test-lift/pre+ g h A)
  (define h+ ((inst lift/pre+ X Y) g))
  (check-true (pmapping-equal? (h+ A) (h A))))

;; The eta testing functions should only be given halting sets if the arrow uses ifte-conv

(: test-eta/bot* (All (X Y) ((Bot-Arrow X Y) (Bot*-Arrow X Y) (Setof (Pair Branch-Trace X)) -> Any)))
(define (test-eta/bot* f f* A)
  (define f** ((inst eta/bot* X Y) f))
  (for: ([tx  (in-list (set->list A))])
    (check-equal? ((f* j0) tx) ((f** j0) tx))))

(: test-eta/map* (All (X Y) ((Map-Arrow X Y) (Map*-Arrow X Y) (Setof (Pair Branch-Trace X)) -> Any)))
(define (test-eta/map* g g* A)
  (define g** ((inst eta/map* X Y) g))
  (check-true (equal? ((g** j0) A) ((g* j0) A))))

(: test-eta/pre* (All (X Y) ((Pre-Arrow X Y) (Pre*-Arrow X Y) (Setof (Pair Branch-Trace X)) -> Any)))
(define (test-eta/pre* h h* A)
  (define h** ((inst eta/pre* X Y) h))
  (check-true (pmapping-equal? ((h** j0) A) ((h* j0) A))))

(: test-eta/bot+ (All (X Y) ((Bot-Arrow X Y) (Bot+-Arrow X Y) (Setof (Pair Branch-Trace X)) -> Any)))
(define (test-eta/bot+ f f+ A)
  (define f++ ((inst eta/bot+ X Y) f))
  (for: ([tx  (in-list (set->list A))])
    (check-equal? (f+ tx) (f++ tx))))

(: test-eta/map+ (All (X Y) ((Map-Arrow X Y) (Map+-Arrow X Y) (Setof (Pair Branch-Trace X)) -> Any)))
(define (test-eta/map+ g g+ A)
  (define g++ ((inst eta/map+ X Y) g))
  (check-true (equal? (g++ A) (g+ A))))

(: test-eta/pre+ (All (X Y) ((Pre-Arrow X Y) (Pre+-Arrow X Y) (Setof (Pair Branch-Trace X)) -> Any)))
(define (test-eta/pre+ h h+ A)
  (define h++ ((inst eta/pre+ X Y) h))
  (check-true (pmapping-equal? (h++ A) (h+ A))))

(printf "Testing add1...~n")
(define add1/bot ((inst arr/bot Integer Integer) add1))
(define add1/map ((inst arr/map Integer Integer) add1))
(define add1/pre ((inst arr/pre Integer Integer) add1))
(define add1/bot* ((inst arr/bot* Integer Integer) add1))
(define add1/map* ((inst arr/map* Integer Integer) add1))
(define add1/pre* ((inst arr/pre* Integer Integer) add1))
(define add1/bot+ ((inst arr/bot+ Integer Integer) add1))
(define add1/map+ ((inst arr/map+ Integer Integer) add1))
(define add1/pre+ ((inst arr/pre+ Integer Integer) add1))
(define add1-domain (set 1 2 3))
(test-lift/map add1/bot add1/map add1-domain)
(test-lift/pre add1/map add1/pre add1-domain)
(define add1-domain* (set-product (set 'leaf) add1-domain))
(test-eta/bot* add1/bot add1/bot* add1-domain*)
(test-lift/map* add1/bot* add1/map* add1-domain*)
(test-lift/pre* add1/map* add1/pre* add1-domain*)
(test-eta/bot+ add1/bot add1/bot+ add1-domain*)
(test-lift/map+ add1/bot+ add1/map+ add1-domain*)
(test-lift/pre+ add1/map+ add1/pre+ add1-domain*)

(printf "Testing sub1...~n")
(define sub1/bot ((inst arr/bot Integer Integer) sub1))
(define sub1/map ((inst arr/map Integer Integer) sub1))
(define sub1/pre ((inst arr/pre Integer Integer) sub1))
(define sub1/bot* ((inst arr/bot* Integer Integer) sub1))
(define sub1/map* ((inst arr/map* Integer Integer) sub1))
(define sub1/pre* ((inst arr/pre* Integer Integer) sub1))
(define sub1/bot+ ((inst arr/bot+ Integer Integer) sub1))
(define sub1/map+ ((inst arr/map+ Integer Integer) sub1))
(define sub1/pre+ ((inst arr/pre+ Integer Integer) sub1))
(define sub1-domain (set 1 2 3))
(test-lift/map sub1/bot sub1/map sub1-domain)
(test-lift/pre sub1/map sub1/pre sub1-domain)
(define sub1-domain* (set-product (set 'leaf) sub1-domain))
(test-eta/bot* sub1/bot sub1/bot* sub1-domain*)
(test-lift/map* sub1/bot* sub1/map* sub1-domain*)
(test-lift/pre* sub1/map* sub1/pre* sub1-domain*)
(test-eta/bot+ sub1/bot sub1/bot+ sub1-domain*)
(test-lift/map+ sub1/bot+ sub1/map+ sub1-domain*)
(test-lift/pre+ sub1/map+ sub1/pre+ sub1-domain*)

(printf "Testing int-id...~n")
(define int-id/bot (>>>/bot add1/bot sub1/bot))
(define int-id/map (>>>/map add1/map sub1/map))
(define int-id/pre (>>>/pre add1/pre sub1/pre))
(define int-id/bot* (>>>/bot* add1/bot* sub1/bot*))
(define int-id/map* (>>>/map* add1/map* sub1/map*))
(define int-id/pre* (>>>/pre* add1/pre* sub1/pre*))
(define int-id/bot+ (>>>/bot+ add1/bot+ sub1/bot+))
(define int-id/map+ (>>>/map+ add1/map+ sub1/map+))
(define int-id/pre+ (>>>/pre+ add1/pre+ sub1/pre+))
(define int-id-domain (set 1 2 3))
(test-lift/map int-id/bot int-id/map int-id-domain)
(test-lift/pre int-id/map int-id/pre int-id-domain)
(define int-id-domain* (set-product (set 'leaf) int-id-domain))
(test-eta/bot* int-id/bot int-id/bot* int-id-domain*)
(test-lift/map* int-id/bot* int-id/map* int-id-domain*)
(test-lift/pre* int-id/map* int-id/pre* int-id-domain*)
(test-eta/bot+ int-id/bot int-id/bot+ int-id-domain*)
(test-lift/map+ int-id/bot+ int-id/map+ int-id-domain*)
(test-lift/pre+ int-id/map+ int-id/pre+ int-id-domain*)

(printf "Testing 1...~n")
(define 1/bot ((inst const/bot Symbol Integer) 1))
(define 1/map ((inst const/map Symbol Integer) 1))
(define 1/pre ((inst const/pre Symbol Integer) 1))
(define 1/bot* ((inst const/bot* Symbol Integer) 1))
(define 1/map* ((inst const/map* Symbol Integer) 1))
(define 1/pre* ((inst const/pre* Symbol Integer) 1))
(define 1/bot+ ((inst const/bot+ Symbol Integer) 1))
(define 1/map+ ((inst const/map+ Symbol Integer) 1))
(define 1/pre+ ((inst const/pre+ Symbol Integer) 1))
(define 1-domain (set 'a 'b 'c))
(test-lift/map 1/bot 1/map 1-domain)
(test-lift/pre 1/map 1/pre 1-domain)
(define 1-domain* (set-product (set 'leaf) 1-domain))
(test-eta/bot* 1/bot 1/bot* 1-domain*)
(test-lift/map* 1/bot* 1/map* 1-domain*)
(test-lift/pre* 1/map* 1/pre* 1-domain*)
(test-eta/bot+ 1/bot 1/bot+ 1-domain*)
(test-lift/map+ 1/bot+ 1/map+ 1-domain*)
(test-lift/pre+ 1/map+ 1/pre+ 1-domain*)

(printf "Testing pair12...~n")
(define pair12/bot (&&&/bot 1/bot ((inst const/bot Symbol Integer) 2)))
(define pair12/map (&&&/map 1/map ((inst const/map Symbol Integer) 2)))
(define pair12/pre (&&&/pre 1/pre ((inst const/pre Symbol Integer) 2)))
(define pair12/bot* (&&&/bot* 1/bot* ((inst const/bot* Symbol Integer) 2)))
(define pair12/map* (&&&/map* 1/map* ((inst const/map* Symbol Integer) 2)))
(define pair12/pre* (&&&/pre* 1/pre* ((inst const/pre* Symbol Integer) 2)))
(define pair12/bot+ (&&&/bot+ 1/bot+ ((inst const/bot+ Symbol Integer) 2)))
(define pair12/map+ (&&&/map+ 1/map+ ((inst const/map+ Symbol Integer) 2)))
(define pair12/pre+ (&&&/pre+ 1/pre+ ((inst const/pre+ Symbol Integer) 2)))
(define pair12-domain (set 'a 'b 'c))
(test-lift/map pair12/bot pair12/map pair12-domain)
(test-lift/pre pair12/map pair12/pre pair12-domain)
(define pair12-domain* (set-product (set 'leaf) pair12-domain))
(test-eta/bot* pair12/bot pair12/bot* pair12-domain*)
(test-lift/map* pair12/bot* pair12/map* pair12-domain*)
(test-lift/pre* pair12/map* pair12/pre* pair12-domain*)
(test-eta/bot+ pair12/bot pair12/bot+ pair12-domain*)
(test-lift/map+ pair12/bot+ pair12/map+ pair12-domain*)
(test-lift/pre+ pair12/map+ pair12/pre+ pair12-domain*)

(printf "Testing int-dup...~n")
(define int-dup/bot (&&&/bot int-id/bot int-id/bot))
(define int-dup/map (&&&/map int-id/map int-id/map))
(define int-dup/pre (&&&/pre int-id/pre int-id/pre))
(define int-dup/bot* (&&&/bot* int-id/bot* int-id/bot*))
(define int-dup/map* (&&&/map* int-id/map* int-id/map*))
(define int-dup/pre* (&&&/pre* int-id/pre* int-id/pre*))
(define int-dup/bot+ (&&&/bot+ int-id/bot+ int-id/bot+))
(define int-dup/map+ (&&&/map+ int-id/map+ int-id/map+))
(define int-dup/pre+ (&&&/pre+ int-id/pre+ int-id/pre+))
(define int-dup-domain (set 1 2 3))
(test-lift/map int-dup/bot int-dup/map int-dup-domain)
(test-lift/pre int-dup/map int-dup/pre int-dup-domain)
(define int-dup-domain* (set-product (set 'leaf) (set 1 2 3)))
(test-eta/bot* int-dup/bot int-dup/bot* int-dup-domain*)
(test-lift/map* int-dup/bot* int-dup/map* int-dup-domain*)
(test-lift/pre* int-dup/map* int-dup/pre* int-dup-domain*)
(test-eta/bot+ int-dup/bot int-dup/bot+ int-dup-domain*)
(test-lift/map+ int-dup/bot+ int-dup/map+ int-dup-domain*)
(test-lift/pre+ int-dup/map+ int-dup/pre+ int-dup-domain*)

(printf "Testing int-pair-id...~n")
(define int-pair-id/bot (&&&/bot (inst fst/bot Integer Integer) (inst snd/bot Integer Integer)))
(define int-pair-id/map (&&&/map (inst fst/map Integer Integer) (inst snd/map Integer Integer)))
(define int-pair-id/pre (&&&/pre (inst fst/pre Integer Integer) (inst snd/pre Integer Integer)))
(define int-pair-id/bot* (&&&/bot* (inst fst/bot* Integer Integer) (inst snd/bot* Integer Integer)))
(define int-pair-id/map* (&&&/map* (inst fst/map* Integer Integer) (inst snd/map* Integer Integer)))
(define int-pair-id/pre* (&&&/pre* (inst fst/pre* Integer Integer) (inst snd/pre* Integer Integer)))
(define int-pair-id/bot+ (&&&/bot+ (inst fst/bot+ Integer Integer) (inst snd/bot+ Integer Integer)))
(define int-pair-id/map+ (&&&/map+ (inst fst/map+ Integer Integer) (inst snd/map+ Integer Integer)))
(define int-pair-id/pre+ (&&&/pre+ (inst fst/pre+ Integer Integer) (inst snd/pre+ Integer Integer)))
(define int-pair-id-domain (set '(1 . 4) '(2 . 5) '(3 . 6)))
(test-lift/map int-pair-id/bot int-pair-id/map int-pair-id-domain)
(test-lift/pre int-pair-id/map int-pair-id/pre int-pair-id-domain)
(define int-pair-id-domain* (set-product (set 'leaf) int-pair-id-domain))
(test-eta/bot* int-pair-id/bot int-pair-id/bot* int-pair-id-domain*)
(test-lift/map* int-pair-id/bot* int-pair-id/map* int-pair-id-domain*)
(test-lift/pre* int-pair-id/map* int-pair-id/pre* int-pair-id-domain*)
(test-eta/bot+ int-pair-id/bot int-pair-id/bot+ int-pair-id-domain*)
(test-lift/map+ int-pair-id/bot+ int-pair-id/map+ int-pair-id-domain*)
(test-lift/pre+ int-pair-id/map+ int-pair-id/pre+ int-pair-id-domain*)

(printf "Testing halt-on-true...~n")

(printf "define halt-on-true/bot~n")
(: halt-on-true/bot (Bot-Arrow Boolean Boolean))
(define halt-on-true/bot
  (ifte/bot (inst id/bot Boolean)
            (lazy/bot (λ () ((inst const/bot Boolean Boolean) #t)))
            (lazy/bot (λ () halt-on-true/bot))))

(printf "define halt-on-true/map~n")
(: halt-on-true/map (Map-Arrow Boolean Boolean))
(define halt-on-true/map
  (ifte/map (inst id/map Boolean)
            (lazy/map (λ () ((inst const/map Boolean Boolean) #t)))
            (lazy/map (λ () halt-on-true/map))))

(printf "define halt-on-true/pre~n")
(: halt-on-true/pre (Pre-Arrow Boolean Boolean))
(define halt-on-true/pre
  (ifte/pre (inst id/pre Boolean)
            (lazy/pre (λ () ((inst const/pre Boolean Boolean) #t)))
            (lazy/pre (λ () halt-on-true/pre))))

(printf "define halt-on-true/bot*~n")
(: halt-on-true/bot* (Bot*-Arrow Boolean Boolean))
(define halt-on-true/bot*
  (ifte-conv/bot* (inst id/bot* Boolean)
                  (lazy/bot* (λ () ((inst const/bot* Boolean Boolean) #t)))
                  (lazy/bot* (λ () halt-on-true/bot*))))

(printf "define halt-on-true/map*~n")
(: halt-on-true/map* (Map*-Arrow Boolean Boolean))
(define halt-on-true/map*
  (ifte-conv/map* (inst id/map* Boolean)
                  (lazy/map* (λ () ((inst const/map* Boolean Boolean) #t)))
                  (lazy/map* (λ () halt-on-true/map*))))

(printf "define halt-on-true/pre*~n")
(: halt-on-true/pre* (Pre*-Arrow Boolean Boolean))
(define halt-on-true/pre*
  (ifte-conv/pre* (inst id/pre* Boolean)
                  (lazy/pre* (λ () ((inst const/pre* Boolean Boolean) #t)))
                  (lazy/pre* (λ () halt-on-true/pre*))))

(printf "define halt-on-true/bot+~n")
(: halt-on-true/bot+ (Bot+-Arrow Boolean Boolean))
(define halt-on-true/bot+
  (ifte-conv/bot+ (inst id/bot+ Boolean)
                  (lazy/bot+ (λ () ((inst const/bot+ Boolean Boolean) #t)))
                  (lazy/bot+ (λ () halt-on-true/bot+))))

(printf "define halt-on-true/map+~n")
(: halt-on-true/map+ (Map+-Arrow Boolean Boolean))
(define halt-on-true/map+
  (ifte-conv/map+ (inst id/map+ Boolean)
                  (lazy/map+ (λ () ((inst const/map+ Boolean Boolean) #t)))
                  (lazy/map+ (λ () halt-on-true/map+))))

(printf "define halt-on-true/pre+~n")
(: halt-on-true/pre+ (Pre+-Arrow Boolean Boolean))
(define halt-on-true/pre+
  (ifte-conv/pre+ (inst id/pre+ Boolean)
                  (lazy/pre+ (λ () ((inst const/pre+ Boolean Boolean) #t)))
                  (lazy/pre+ (λ () halt-on-true/pre+))))

(define halt-on-true-domain (set #t))
(printf "test-lift/map~n")
(test-lift/map halt-on-true/bot halt-on-true/map halt-on-true-domain)
(printf "test-lift/pre~n")
(test-lift/pre halt-on-true/map halt-on-true/pre halt-on-true-domain)

(define halt-on-true-domain*
  (set-product (list->set (take (shuffle (set->list some-traces)) 20))
               (set #t #f)))
(printf "test-lift/map*~n")
(test-lift/map* halt-on-true/bot* halt-on-true/map* halt-on-true-domain*)
(printf "test-lift/pre*~n")
(test-lift/pre* halt-on-true/map* halt-on-true/pre* halt-on-true-domain*)
(printf "test-lift/map+~n")
(test-lift/map+ halt-on-true/bot+ halt-on-true/map+ halt-on-true-domain*)
(printf "test-lift/pre+~n")
(test-lift/pre+ halt-on-true/map+ halt-on-true/pre+ halt-on-true-domain*)

(define halt-on-true-halting-set*
  (pmapping-ap ((halt-on-true/pre* j0) halt-on-true-domain*) (set #t #f)))
(printf "test-eta/bot*~n")
(test-eta/bot* halt-on-true/bot halt-on-true/bot* halt-on-true-halting-set*)
(printf "test-eta/map*~n")
(test-eta/map* halt-on-true/map halt-on-true/map* halt-on-true-halting-set*)
(printf "test-eta/pre*~n")
(test-eta/pre* halt-on-true/pre halt-on-true/pre* halt-on-true-halting-set*)

(define halt-on-true-halting-set+
  (pmapping-ap (halt-on-true/pre+ halt-on-true-domain*) (set #t #f)))
(printf "test-eta/bot+~n")
(test-eta/bot+ halt-on-true/bot halt-on-true/bot+ halt-on-true-halting-set+)
(printf "test-eta/map+~n")
(test-eta/map+ halt-on-true/map halt-on-true/map+ halt-on-true-halting-set+)
(printf "test-eta/pre+~n")
(test-eta/pre+ halt-on-true/pre halt-on-true/pre+ halt-on-true-halting-set+)

(check-true (equal? halt-on-true-halting-set* halt-on-true-halting-set+))

#|
(: error/bot (Bot-Arrow Boolean Nothing))
(define (error/bot a) ⊥)

(define error/map ((inst lift/map Boolean Nothing) error/bot))
(define error/pre ((inst lift/pre Boolean Nothing) error/map))

(define error/bot* (eta/bot* error/bot))
(define error/map* (eta/map* error/map))
(define error/pre* (eta/pre* error/pre))

(define error/bot+ (eta/bot+ error/bot))
(define error/map+ (eta/map+ error/map))
(define error/pre+ (eta/pre+ error/pre))

;; Should terminate
((&&&/bot error/bot halt-on-true/bot) true)
;; Should not terminate
((&&&/bot error/bot halt-on-true/bot) false)

;; Should terminate
((&&&/map error/map halt-on-true/map)
 (set true))
;; Should not terminate
((&&&/map error/map halt-on-true/map)
 (set false))
|#
