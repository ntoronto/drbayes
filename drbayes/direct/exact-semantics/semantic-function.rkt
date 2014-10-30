#lang typed/racket

(require typed/rackunit
         "../types.rkt"
         "fun-arrow.rkt"
         "bot-arrow.rkt"
         "partial-arrows.rkt"
         )

#;
(begin
  (define-type (Arrow X Y) (Bot-Arrow X Y))
  (define arr/a arr/bot)
  (define >>>/a >>>/bot)
  (define &&&/a &&&/bot)
  (define lazy/a lazy/bot)
  (define ifte/a ifte/bot)
  
  (: run/a (All (X Y) ((Bot-Arrow Null Y) -> (U Bottom Y))))
  (define (run/a f)
    (unjust (f null)))
  )

(begin
  (define-type (Arrow X Y) (Bot*-Arrow X Y))
  (define arr/a arr/bot*)
  (define >>>/a >>>/bot*)
  (define &&&/a &&&/bot*)
  (define lazy/a lazy/bot*)
  (define ifte/a ifte/bot*)
  
  (: run/a (All (X Y) ((Bot*-Arrow Null Y) -> (U Bottom Y))))
  (define (run/a f)
    (unjust (ap/bot* f null)))
  )

(define-syntax (env stx) (raise-syntax-error 'env "cannot be used as an expression" stx))

(: plus ((Pair Number Number) -> Number))
(define (plus ab)
  (+ (car ab) (cdr ab)))

(: minus ((Pair Number Number) -> Number))
(define (minus ab)
  (- (car ab) (cdr ab)))

(: neg (Number -> Number))
(define (neg x) (- x))

(: let/a (All (X Y) ((Arrow X Y) -> (All (Z) (Arrow (Pair Y X) Z) -> (Arrow X Z)))))
(define ((let/a expr) body)
  ((expr . &&&/a . (arr/a (inst id X))) . >>>/a . body))

(define-for-syntax (S stx)
  ;(printf "stx = ~v~n" (syntax->datum stx))
  (syntax-case stx (cons car cdr if lazy let env Pair + - neg)
    [(cons e1 e2)
     #`(#,(S #'e1) . &&&/a . #,(S #'e2))]
    [(car (Pair X Y) e)
     #`(#,(S #'e) . >>>/a . (arr/a (inst fst X Y)))]
    [(cdr (Pair X Y) e)
     #`(#,(S #'e) . >>>/a . (arr/a (inst snd X Y)))]
    [(if ec et ef)
     #`(ifte/a #,(S #'ec) #,(S #'(lazy et)) #,(S #'(lazy ef)))]
    [(lazy e)
     #`(lazy/a (λ () #,(S #'e)))]
    [(let E ex X eb)
     #`(((inst let/a E X) #,(S #'ex)) #,(S #'eb))]
    [(env (Pair E0 E) 0)
     #`(arr/a (inst fst E0 E))]
    [(env (Pair E0 E) n)
     (exact-nonnegative-integer? (syntax->datum #'n))
     (let ([n  (syntax->datum #'n)])
       #`((arr/a (inst snd E0 E)) . >>>/a . #,(S #`(env E #,(- n 1)))))]
    [(+ e1 e2)
     #`((#,(S #'e1) . &&&/a . #,(S #'e2)) . >>>/a . (arr/a plus))]
    [(- e1 e2)
     #`((#,(S #'e1) . &&&/a . #,(S #'e2)) . >>>/a . (arr/a minus))]
    [(neg e)
     #`(#,(S #'e) . >>>/a . (arr/a neg))]
    [(ef ex)
     #`(#,(S #'(cons ex null)) . >>>/a . ef)]
    [v
     #`(arr/a (const v))]
    ))

(define-syntax (meaningof stx)
  (syntax-case stx ()
    [(_ e)  (S #'e)]))

(check-equal? (run/a (meaningof 4)) 4)
(check-equal? (run/a (meaningof (+ 4 5))) 9)
(check-equal? (run/a (meaningof (cons 4 5))) (cons 4 5))
(check-equal? (run/a (meaningof (if #t 4 5))) 4)
(check-equal? (run/a (meaningof (if #f 4 5))) 5)

(check-equal? (run/a (meaningof (let Null 4 Number 5)))
              5)

(check-equal? (run/a (meaningof (let Null 4 Number
                                  (env (Pair Number Null) 0))))
              4)

(check-equal? (run/a (meaningof (let Null 4 Number
                                  (let (Pair Number Null) 5 Number
                                    (- (env (Pair Number (Pair Number Null)) 0)
                                       (env (Pair Number (Pair Number Null)) 1))))))
              1)

(define negate (meaningof (neg (env (Pair Number Null) 0))))
(check-equal? (run/a (meaningof (negate 4))) -4)


(define sub (meaningof (- (car (Pair Number Number) (env (Pair (Pair Number Number) Null) 0))
                          (cdr (Pair Number Number) (env (Pair (Pair Number Number) Null) 0)))))
(check-equal? (run/a (meaningof (sub (cons 4 5)))) -1)
