#lang typed/racket/base

(require racket/list
         racket/match
         racket/set
         "../untyped-utils.rkt")

(provide (all-defined-out))

(define-singleton-type Empty-Symbol-Set empty-symbol-set)
(define-singleton-type Full-Symbol-Set full-symbol-set)

(struct: top-symbol ([value : Symbol]) #:transparent)
(struct: bot-symbol-set ([values : (Setof Symbol)]) #:transparent)
(struct: top-symbol-set ([values : (Setof Symbol)]) #:transparent)

(define-type Nonextremal-Symbol-Set (U Symbol bot-symbol-set top-symbol top-symbol-set))
(define-type Nonfull-Symbol-Set (U Nonextremal-Symbol-Set Empty-Symbol-Set))
(define-type Nonempty-Symbol-Set (U Nonextremal-Symbol-Set Full-Symbol-Set))
(define-type Symbol-Set (U Nonextremal-Symbol-Set Full-Symbol-Set Empty-Symbol-Set))

(: symbol-set? (Any -> Boolean : Symbol-Set))
(define (symbol-set? v)
  (or (symbol? v)
      (bot-symbol-set? v)
      (empty-symbol-set? v)
      (top-symbol? v)
      (top-symbol-set? v)
      (full-symbol-set? v)))

;; ===================================================================================================

(: symbol-set-member? (Symbol-Set Symbol -> Boolean))
(define (symbol-set-member? A x)
  (cond [(empty-symbol-set? A)  #f]
        [(full-symbol-set? A)  #t]
        [(symbol? A)  (eq? A x)]
        [(top-symbol? A)  (not (eq? (top-symbol-value A) x))]
        [(bot-symbol-set? A)  (set-member? (bot-symbol-set-values A) x)]
        [(top-symbol-set? A)  (not (set-member? (top-symbol-set-values A) x))]))

;; ===================================================================================================

(: set->bot-symbol-set ((Setof Symbol) -> (U Empty-Symbol-Set Symbol bot-symbol-set)))
(define (set->bot-symbol-set s)
  (cond [(set-empty? s)  empty-symbol-set]
        [(= 1 (set-count s))  (set-first s)]
        [else  (bot-symbol-set s)]))

(: set->top-symbol-set ((Setof Symbol) -> (U Full-Symbol-Set top-symbol top-symbol-set)))
(define (set->top-symbol-set s)
  (cond [(set-empty? s)  full-symbol-set]
        [(= 1 (set-count s))  (top-symbol (set-first s))]
        [else  (top-symbol-set s)]))

(: expand-symbol-set (Nonextremal-Symbol-Set -> (U bot-symbol-set top-symbol-set)))
(define (expand-symbol-set A)
  (cond [(symbol? A)  (bot-symbol-set (set A))]
        [(top-symbol? A)  (top-symbol-set (set (top-symbol-value A)))]
        [else  A]))

(: symbol-set (Symbol * -> (U Empty-Symbol-Set Symbol bot-symbol-set)))
(define (symbol-set . lst)
  (set->bot-symbol-set (list->set lst)))

;; ===================================================================================================
;; Union

(: nonextremal-symbol-set-union
   (Nonextremal-Symbol-Set Nonextremal-Symbol-Set -> Nonempty-Symbol-Set))
(define (nonextremal-symbol-set-union A B)
  (cond [(and (symbol? A) (symbol? B))
         (if (eq? A B) A (bot-symbol-set (set A B)))]
        [(and (symbol? A) (top-symbol? B))
         (if (eq? A (top-symbol-value B)) full-symbol-set B)]
        [(and (top-symbol? A) (symbol? B))
         (if (eq? B (top-symbol-value A)) full-symbol-set A)]
        [(and (top-symbol? A) (top-symbol? B))
         (if (eq? (top-symbol-value A) (top-symbol-value B)) A full-symbol-set)]
        [else
         (let ([A  (expand-symbol-set A)]
               [B  (expand-symbol-set B)])
           (cond [(and (bot-symbol-set? A) (bot-symbol-set? B))  (bot-bot-union A B)]
                 [(and (bot-symbol-set? A) (top-symbol-set? B))  (bot-top-union A B)]
                 [(and (top-symbol-set? A) (bot-symbol-set? B))  (bot-top-union B A)]
                 [(and (top-symbol-set? A) (top-symbol-set? B))  (top-top-union A B)]))]))

(: symbol-set-union (case-> (Symbol-Set Nonempty-Symbol-Set -> Nonempty-Symbol-Set)
                            (Nonempty-Symbol-Set Symbol-Set -> Nonempty-Symbol-Set)
                            (Symbol-Set Symbol-Set -> Symbol-Set)))
(define (symbol-set-union A B)
  (cond [(empty-symbol-set? A)  B]
        [(empty-symbol-set? B)  A]
        [(full-symbol-set? A)  A]
        [(full-symbol-set? B)  B]
        [else  (nonextremal-symbol-set-union A B)]))

(: bot-bot-union (bot-symbol-set bot-symbol-set -> bot-symbol-set))
(define (bot-bot-union A B)
  (bot-symbol-set
   (set-union (bot-symbol-set-values A)
              (bot-symbol-set-values B))))

(: bot-top-union (bot-symbol-set top-symbol-set -> (U Full-Symbol-Set top-symbol top-symbol-set)))
(define (bot-top-union A B)
  (set->top-symbol-set
   (set-subtract (top-symbol-set-values B)
                 (bot-symbol-set-values A))))

(: top-top-union (top-symbol-set top-symbol-set -> (U Full-Symbol-Set top-symbol top-symbol-set)))
(define (top-top-union A B)
  (set->top-symbol-set
   (set-intersect (top-symbol-set-values A)
                  (top-symbol-set-values B))))

;; ===================================================================================================
;; Intersection

(: nonextremal-symbol-set-intersect
   (Nonextremal-Symbol-Set Nonextremal-Symbol-Set -> Nonfull-Symbol-Set))
(define (nonextremal-symbol-set-intersect A B)
  (cond [(and (symbol? A) (symbol? B))
         (if (eq? A B) A empty-symbol-set)]
        [(and (symbol? A) (top-symbol? B))
         (if (eq? A (top-symbol-value B)) empty-symbol-set A)]
        [(and (top-symbol? A) (symbol? B))
         (if (eq? (top-symbol-value A) B) empty-symbol-set B)]
        [(and (top-symbol? A) (top-symbol? B))
         (let ([x  (top-symbol-value A)]
               [y  (top-symbol-value B)])
           (if (eq? x y) A (top-symbol-set (set x y))))]
        [else
         (let ([A  (expand-symbol-set A)]
               [B  (expand-symbol-set B)])
           (cond [(and (bot-symbol-set? A) (bot-symbol-set? B))  (bot-bot-intersect A B)]
                 [(and (bot-symbol-set? A) (top-symbol-set? B))  (bot-top-intersect A B)]
                 [(and (top-symbol-set? A) (bot-symbol-set? B))  (bot-top-intersect B A)]
                 [(and (top-symbol-set? A) (top-symbol-set? B))  (top-top-intersect A B)]))]))

(: symbol-set-intersect (case-> (Nonfull-Symbol-Set Symbol-Set -> Nonfull-Symbol-Set)
                                (Symbol-Set Nonfull-Symbol-Set -> Nonfull-Symbol-Set)
                                (Symbol-Set Symbol-Set -> Symbol-Set)))
(define (symbol-set-intersect A B)
  (cond [(empty-symbol-set? A)  A]
        [(empty-symbol-set? B)  B]
        [(full-symbol-set? A)  B]
        [(full-symbol-set? B)  A]
        [else  (nonextremal-symbol-set-intersect A B)]))

(: bot-bot-intersect (bot-symbol-set bot-symbol-set -> (U Empty-Symbol-Set Symbol bot-symbol-set)))
(define (bot-bot-intersect A B)
  (set->bot-symbol-set
   (set-intersect (bot-symbol-set-values A)
                  (bot-symbol-set-values B))))

(: bot-top-intersect (bot-symbol-set top-symbol-set -> (U Empty-Symbol-Set Symbol bot-symbol-set)))
(define (bot-top-intersect A B)
  (set->bot-symbol-set
   (set-subtract (bot-symbol-set-values A)
                 (top-symbol-set-values B))))

(: top-top-intersect (top-symbol-set top-symbol-set -> top-symbol-set))
(define (top-top-intersect A B)
  (top-symbol-set
   (set-union (top-symbol-set-values A)
              (top-symbol-set-values B))))

;; ===================================================================================================
;; Complement

(: nonextremal-symbol-set-complement (Nonextremal-Symbol-Set -> Nonextremal-Symbol-Set))
(define (nonextremal-symbol-set-complement A)
  (cond [(symbol? A)  (top-symbol A)]
        [(top-symbol? A)  (top-symbol-value A)]
        [(bot-symbol-set? A)  (top-symbol-set (bot-symbol-set-values A))]
        [(top-symbol-set? A)  (bot-symbol-set (top-symbol-set-values A))]))

(: symbol-set-complement (case-> (Nonextremal-Symbol-Set -> Nonextremal-Symbol-Set)
                                 (Nonempty-Symbol-Set -> Nonfull-Symbol-Set)
                                 (Nonfull-Symbol-Set -> Nonempty-Symbol-Set)
                                 (Symbol-Set -> Symbol-Set)))
(define (symbol-set-complement A)
  (cond [(empty-symbol-set? A)  full-symbol-set]
        [(full-symbol-set? A)   empty-symbol-set]
        [else  (nonextremal-symbol-set-complement A)]))

;; ===================================================================================================
;; Difference

(: nonextremal-symbol-set-subtract
   (Nonextremal-Symbol-Set Nonextremal-Symbol-Set -> Nonfull-Symbol-Set))
(define (nonextremal-symbol-set-subtract A B)
  (cond [(and (symbol? A) (symbol? B))
         (if (eq? A B) empty-symbol-set A)]
        [(and (symbol? A) (top-symbol? B))
         (if (eq? A (top-symbol-value B)) A empty-symbol-set)]
        [(and (top-symbol? A) (symbol? B))
         (let ([a  (top-symbol-value A)])
           (if (eq? a B) A (top-symbol-set (set a B))))]
        [(and (top-symbol? A) (top-symbol? B))
         (let ([b  (top-symbol-value B)])
           (if (eq? (top-symbol-value A) b) empty-symbol-set b))]
        [else
         (let ([A  (expand-symbol-set A)]
               [B  (expand-symbol-set B)])
           (cond [(and (bot-symbol-set? A) (bot-symbol-set? B))  (bot-bot-subtract A B)]
                 [(and (bot-symbol-set? A) (top-symbol-set? B))  (bot-top-subtract A B)]
                 [(and (top-symbol-set? A) (bot-symbol-set? B))  (top-bot-subtract A B)]
                 [(and (top-symbol-set? A) (top-symbol-set? B))  (top-top-subtract A B)]))]))

(: symbol-set-subtract (case-> (Full-Symbol-Set Nonextremal-Symbol-Set -> Nonextremal-Symbol-Set)
                               (Full-Symbol-Set Nonfull-Symbol-Set -> Nonempty-Symbol-Set)
                               (Symbol-Set Nonempty-Symbol-Set -> Nonfull-Symbol-Set)
                               (Symbol-Set Symbol-Set -> Symbol-Set)))
(define (symbol-set-subtract A B)
  (cond [(empty-symbol-set? B)  A]
        [(empty-symbol-set? A)  empty-symbol-set]
        [(full-symbol-set? B)  empty-symbol-set]
        [(full-symbol-set? A)  (nonextremal-symbol-set-complement B)]
        [else  (nonextremal-symbol-set-subtract A B)]))

(: bot-bot-subtract (bot-symbol-set bot-symbol-set -> (U Empty-Symbol-Set Symbol bot-symbol-set)))
(define (bot-bot-subtract A B)
  (set->bot-symbol-set
   (set-subtract (bot-symbol-set-values A)
                 (bot-symbol-set-values B))))

(: bot-top-subtract (bot-symbol-set top-symbol-set -> (U Empty-Symbol-Set Symbol bot-symbol-set)))
(define (bot-top-subtract A B)
  (set->bot-symbol-set
   (set-intersect (bot-symbol-set-values A)
                  (top-symbol-set-values B))))

(: top-bot-subtract (top-symbol-set bot-symbol-set -> top-symbol-set))
(define (top-bot-subtract A B)
  (top-symbol-set
   (set-union (top-symbol-set-values A)
              (bot-symbol-set-values B))))

(: top-top-subtract (top-symbol-set top-symbol-set -> (U Empty-Symbol-Set Symbol bot-symbol-set)))
(define (top-top-subtract A B)
  (set->bot-symbol-set
   (set-subtract (top-symbol-set-values B)
                 (top-symbol-set-values A))))

;; ===================================================================================================
;; Subset

(: symbol-set-subseteq? (Symbol-Set Symbol-Set -> Boolean))
(define (symbol-set-subseteq? A B)
  (cond [(empty-symbol-set? A)  #t]
        [(empty-symbol-set? B)  #f]
        [(full-symbol-set? B)  #t]
        [(full-symbol-set? A)  #f]
        [else
         (let ([A  (expand-symbol-set A)]
               [B  (expand-symbol-set B)])
           (cond [(and (bot-symbol-set? A) (bot-symbol-set? B))  (bot-bot-subseteq? A B)]
                 [(and (bot-symbol-set? A) (top-symbol-set? B))  (bot-top-subseteq? A B)]
                 [(and (top-symbol-set? A) (bot-symbol-set? B))  (top-bot-subseteq? A B)]
                 [(and (top-symbol-set? A) (top-symbol-set? B))  (top-top-subseteq? A B)]))]))

(: bot-bot-subseteq? (bot-symbol-set bot-symbol-set -> Boolean))
(define (bot-bot-subseteq? A B)
  (subset? (bot-symbol-set-values A)
           (bot-symbol-set-values B)))

(: bot-top-subseteq? (bot-symbol-set top-symbol-set -> Boolean))
(define (bot-top-subseteq? A B)
  (set-empty? (set-intersect (bot-symbol-set-values A)
                             (top-symbol-set-values B))))

(: top-bot-subseteq? (top-symbol-set bot-symbol-set -> Boolean))
(define (top-bot-subseteq? A B) #f)

(: top-top-subseteq? (top-symbol-set top-symbol-set -> Boolean))
(define (top-top-subseteq? A B)
  (subset? (top-symbol-set-values B)
           (top-symbol-set-values A)))
