#lang typed/racket/base

(require (for-syntax racket/base)
         racket/list
         "types.rkt"
         "real-set.rkt"
         "null-set.rkt"
         "bool-set.rkt"
         "pair-set.rkt"
         "tree-set.rkt"
         "tree-value.rkt"
         "extremal-set.rkt"
         "union.rkt"
         "value.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Basic set ops

(define-singleton-type Different different)

(: basic-member? (Basic Value -> (U Different Boolean)))
(define (basic-member? A x)
  (cond [(and (real-set? A) (flonum? x))   (real-set-member? A x)]
        [(and (bool-set? A) (boolean? x))  (bool-set-member? A x)]
        [(and (null-set? A) (null? x))     (null-set-member? A x)]
        [(and (pair-set? A) (pair? x))     (pair-set-member? A x)]
        [(and (omega-set? A) (omega? x))   (omega-set-member? A x)]
        [(and (trace-set? A) (trace? x))   (trace-set-member? A x)]
        [else  different]))

(: basic-intersect (case-> (Nonfull-Basic Basic -> (U Different Nonfull-Basic))
                           (Basic Nonfull-Basic -> (U Different Nonfull-Basic))
                           (Basic Basic -> (U Different Basic))))
(define (basic-intersect A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-intersect A B)]
        [(and (null-set? A) (null-set? B))  (null-set-intersect A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-intersect A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-intersect A B)]
        [(and (omega-set? A) (omega-set? B))  (omega-set-intersect A B)]
        [(and (trace-set? A) (trace-set? B))  (trace-set-intersect A B)]
        [else  different]))

(: basic-join (case-> (Nonempty-Basic Basic -> (U Different Nonempty-Basic))
                      (Basic Nonempty-Basic -> (U Different Nonempty-Basic))
                      (Basic Basic -> (U Different Basic))))
(define (basic-join A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-union A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-union A B)]
        [(and (null-set? A) (null-set? B))  (null-set-union A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-join A B)]
        [(and (omega-set? A) (omega-set? B))  (omega-set-join A B)]
        [(and (trace-set? A) (trace-set? B))  (trace-set-join A B)]
        [else  different]))

(: basic-subseteq? (Basic Basic -> (U Different Boolean)))
(define (basic-subseteq? A B)
  (cond [(and (real-set? A) (real-set? B))  (real-set-subseteq? A B)]
        [(and (bool-set? A) (bool-set? B))  (bool-set-subseteq? A B)]
        [(and (null-set? A) (null-set? B))  (null-set-subseteq? A B)]
        [(and (pair-set? A) (pair-set? B))  (pair-set-subseteq? A B)]
        [(and (omega-set? A) (omega-set? B))  (omega-set-subseteq? A B)]
        [(and (trace-set? A) (trace-set? B))  (trace-set-subseteq? A B)]
        [else  different]))

;; ===================================================================================================
;; Membership

(: set-member? (Set Maybe-Value -> Boolean))
(define (set-member? A x)
  (cond [(bottom? x)       #f]
        [(empty-set? A)    #f]
        [(not (value? x))  #f]
        [(universe? A)     #t]
        [(bot-set? A)
         (cond [(bot-basic? A)  (bot-basic-member? A x)]
               [(bot-tagged? A)  (bot-tagged-member? A x)]
               [else  (bot-union-member? A x)])]
        [else
         (cond [(top-basic? A)  (top-basic-member? A x)]
               [(top-tagged? A)  (top-tagged-member? A x)]
               [else  (top-union-member? A x)])]))

(: bot-basic-member? (Bot-Basic Value -> Boolean))
(define (bot-basic-member? A x)
  (define res (basic-member? A x))
  (if (different? res) #f res))

(: top-basic-member? (Top-Basic Value -> Boolean))
(define (top-basic-member? A x)
  (define Asub (top-basic-set A))
  (define res (basic-member? Asub x))
  (if (different? res) #t res))

(: bot-tagged-member? (Bot-Tagged Value -> Boolean))
(define (bot-tagged-member? A x)
  (and (tagged-value? x)
       (eq? (bot-tagged-tag A) (tagged-value-tag x))
       (set-member? (bot-tagged-set A) (tagged-value-value x))))

(: top-tagged-member? (Top-Tagged Value -> Boolean))
(define (top-tagged-member? A x)
  (or (not (tagged-value? x))
      (not (eq? (top-tagged-tag A) (tagged-value-tag x)))
      (set-member? (top-tagged-set A) (tagged-value-value x))))

(: bot-union-member? (Bot-Union Value -> Boolean))
(define (bot-union-member? A x)
  (set-member? (bot-union-ref A (value-tag x)) x))

(: top-union-member? (Top-Union Value -> Boolean))
(define (top-union-member? A x)
  (set-member? (top-union-ref A (value-tag x)) x))

;; ===================================================================================================
;; Join

(: set-join (case-> (Set Nonempty-Set -> Nonempty-Set)
                    (Nonempty-Set Set -> Nonempty-Set)
                    (Set Set -> Set)))
(define (set-join A B)
  (cond
    [(empty-set? A)  B]
    [(empty-set? B)  A]
    [(eq? A B)  B]
    [(universe? A)   A]
    [(universe? B)   B]
    [(bot-set? A)
     (if (bot-set? B)
         (if (and (bot-entry? A) (bot-entry? B)) (bot-bot-entry-join A B) (bot-bot-join A B))
         (if (and (bot-entry? A) (top-entry? B)) (top-bot-entry-join B A) (top-bot-join B A)))]
    [else
     (if (bot-set? B)
         (if (and (top-entry? A) (bot-entry? B)) (top-bot-entry-join A B) (top-bot-join A B))
         (if (and (top-entry? A) (top-entry? B)) (top-top-entry-join B A) (top-top-join B A)))]))

(: bot-bot-join ((U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union) -> Nonempty-Set))
(define (bot-bot-join A B)
  (for/fold ([A A]) ([Bsub  (in-list (bot-union-sets B))])
    (define b-tag (bot-tag Bsub))
    (define Asub (bot-union-ref A b-tag))
    (define Dsub (if (empty-set? Asub) Bsub (bot-bot-entry-join Asub Bsub)))
    (bot-union-add A Dsub)))

(: top-bot-join ((U Top-Entry Top-Union) (U Bot-Entry Bot-Union) -> Nonempty-Set))
(define (top-bot-join A B)
  (for/fold: ([A : (U Universe Top-Entry Top-Union)  A]) ([Asub  (in-list (top-union-sets A))])
    (define a-tag (top-tag Asub))
    (define Bsub (bot-union-ref B a-tag))
    (define Dsub (if (empty-set? Bsub) Asub (top-bot-entry-join Asub Bsub)))
    (if (universe? Dsub) (top-union-remove A a-tag) (top-union-add A Dsub))))

(: top-top-join ((U Top-Entry Top-Union) (U Top-Entry Top-Union) -> Nonempty-Set))
(define (top-top-join A B)
  (for/fold: ([C : (U Universe Top-Entry Top-Union)  universe]) ([Bsub  (in-list (top-union-sets B))])
    (define b-tag (top-tag Bsub))
    (define Asub (top-union-ref A b-tag))
    (define Dsub (if (universe? Asub) universe (top-top-entry-join Asub Bsub)))
    (if (universe? Dsub) C (top-union-add C Dsub))))

(: bot-bot-entry-join (Bot-Entry Bot-Entry -> (U Bot-Entry Bot-Union)))
(define (bot-bot-entry-join A B)
  (cond [(and (bot-basic? A)  (bot-basic? B))  (bot-bot-basic-join A B)]
        [(and (bot-tagged? A) (bot-tagged? B)  (bot-bot-tagged-join A B))]
        [else  (bot-union A B)]))

(: top-bot-entry-join (Top-Entry Bot-Entry -> (U Top-Entry Top-Union Universe)))
(define (top-bot-entry-join A B)
  (cond [(and (top-basic? A)  (bot-basic? B))   (top-bot-basic-join A B)]
        [(and (top-tagged? A) (bot-tagged? B))  (top-bot-tagged-join A B)]
        [else  A]))

(: top-top-entry-join (Top-Entry Top-Entry -> (U Top-Entry Top-Union Universe)))
(define (top-top-entry-join A B)
  (cond [(and (top-basic? A)  (top-basic? B))   (top-top-basic-join A B)]
        [(and (top-tagged? A) (top-tagged? B))  (top-top-tagged-join A B)]
        [else  universe]))

(: bot-bot-basic-join (Bot-Basic Bot-Basic -> (U Bot-Basic Bot-Union)))
(define (bot-bot-basic-join A B)
  (define C (basic-join A B))
  (cond [(different? C)  (bot-union A B)]
        [(eq? C A)  A]
        [(eq? C B)  B]
        [else  C]))

(: top-bot-basic-join (Top-Basic Bot-Basic -> (U Top-Basic Top-Union Universe)))
(define (top-bot-basic-join A B)
  (define Asub (top-basic-set A))
  (define Csub (basic-join Asub B))
  (cond [(different? Csub)  A]
        [(eq? Csub Asub)  A]
        [else  (top-basic Csub)]))

(: top-top-basic-join (Top-Basic Top-Basic -> (U Top-Basic Top-Union Universe)))
(define (top-top-basic-join A B)
  (define Asub (top-basic-set A))
  (define Bsub (top-basic-set B))
  (define Csub (basic-join Asub Bsub))
  (cond [(different? Csub)  universe]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (top-basic Csub)]))

(: bot-bot-tagged-join (Bot-Tagged Bot-Tagged -> (U Bot-Tagged Bot-Union)))
(define (bot-bot-tagged-join A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define Csub (set-join Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (bot-tagged a-tag Csub)])]
        [else  (bot-union A B)]))

(: top-bot-tagged-join (Top-Tagged Bot-Tagged -> (U Top-Tagged Top-Union Universe)))
(define (top-bot-tagged-join A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define Csub (set-join Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [else  (top-tagged a-tag Csub)])]
        [else  A]))

(: top-top-tagged-join (Top-Tagged Top-Tagged -> (U Top-Tagged Top-Union Universe)))
(define (top-top-tagged-join A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-join Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (top-tagged a-tag Csub)])]
        [else  universe]))

;; ===================================================================================================
;; Intersection

(: set-intersect (case-> (Set Nonfull-Set -> Nonfull-Set)
                         (Nonfull-Set Set -> Nonfull-Set)
                         (Set Set -> Set)))
(define (set-intersect A B)
  (cond [(universe? A)   B]
        [(universe? B)   A]
        [(eq? A B)  B]
        [(empty-set? A)  A]
        [(empty-set? B)  B]
        [(bot-set? A)
         (if (bot-set? B)
             (cond [(and (bot-entry? A) (bot-entry? B))  (bot-bot-entry-intersect A B)]
                   [else  (bot-bot-intersect A B)])
             (cond [(and (bot-entry? A) (top-entry? B))  (bot-top-entry-intersect A B)]
                   [else  (bot-top-intersect A B)]))]
        [else
         (if (bot-set? B)
             (cond [(and (top-entry? A) (bot-entry? B))  (bot-top-entry-intersect B A)]
                   [else  (bot-top-intersect B A)])
             (cond [(and (top-entry? A) (top-entry? B))  (top-top-entry-intersect A B)]
                   [else  (top-top-intersect A B)]))]))

(: bot-bot-intersect ((U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union)
                                              -> (U Empty-Set Bot-Entry Bot-Union)))
(define (bot-bot-intersect A B)
  (for/fold: ([C : (U Empty-Set Bot-Entry Bot-Union)  empty-set]
              ) ([Bsub  (in-list (bot-union-sets B))])
    (define b-tag (bot-tag Bsub))
    (define Asub (bot-union-ref A b-tag))
    (define Dsub (if (empty-set? Asub) empty-set (bot-bot-entry-intersect Asub Bsub)))
    (if (empty-set? Dsub) C (bot-union-add C Dsub))))

(: bot-top-intersect ((U Bot-Entry Bot-Union) (U Top-Entry Top-Union)
                                              -> (U Bot-Entry Bot-Union Empty-Set)))
(define (bot-top-intersect A B)
  (for/fold: ([A : (U Empty-Set Bot-Entry Bot-Union)  A]) ([Asub  (in-list (bot-union-sets A))])
    (define a-tag (bot-tag Asub))
    (define Bsub (top-union-ref B a-tag))
    (define Dsub (if (universe? Bsub) Asub (bot-top-entry-intersect Asub Bsub)))
    (if (empty-set? Dsub) (bot-union-remove A a-tag) (bot-union-add A Dsub))))

(: top-top-intersect ((U Top-Entry Top-Union) (U Top-Entry Top-Union) -> (U Top-Entry Top-Union)))
(define (top-top-intersect A B)
  (for/fold ([A A]) ([Bsub  (in-list (top-union-sets B))])
    (define b-tag (top-tag Bsub))
    (define Asub (top-union-ref A b-tag))
    (define Dsub (if (universe? Asub) Bsub (top-top-entry-intersect Asub Bsub)))
    (top-union-add A Dsub)))

(: bot-bot-entry-intersect (Bot-Entry Bot-Entry -> (U Bot-Entry Empty-Set)))
(define (bot-bot-entry-intersect A B)
  (cond [(and (bot-basic? A)  (bot-basic? B))   (bot-bot-basic-intersect A B)]
        [(and (bot-tagged? A) (bot-tagged? B))  (bot-bot-tagged-intersect A B)]
        [else  empty-set]))

(: bot-top-entry-intersect (Bot-Entry Top-Entry -> (U Bot-Entry Empty-Set)))
(define (bot-top-entry-intersect A B)
  (cond [(and (bot-basic? A)  (top-basic? B))   (bot-top-basic-intersect A B)]
        [(and (bot-tagged? A) (top-tagged? B))  (bot-top-tagged-intersect A B)]
        [else  A]))

(: top-top-entry-intersect (Top-Entry Top-Entry -> (U Top-Entry Top-Union)))
(define (top-top-entry-intersect A B)
  (cond [(and (top-basic? A)  (top-basic? B))   (top-top-basic-intersect A B)]
        [(and (top-tagged? A) (top-tagged? B))  (top-top-tagged-intersect A B)]
        [else  (top-union A B)]))

(: bot-bot-basic-intersect (Bot-Basic Bot-Basic -> (U Bot-Basic Empty-Set)))
(define (bot-bot-basic-intersect A B)
  (define C (basic-intersect A B))
  (cond [(different? C)  empty-set]
        [else  (bot-basic C)]))

(: bot-top-basic-intersect (Bot-Basic Top-Basic -> (U Bot-Basic Empty-Set)))
(define (bot-top-basic-intersect A B)
  (define Bsub (top-basic-set B))
  (define Csub (basic-intersect A Bsub))
  (cond [(different? Csub)  A]
        [else  (bot-basic Csub)]))

(: top-top-basic-intersect (Top-Basic Top-Basic -> (U Top-Basic Top-Union)))
(define (top-top-basic-intersect A B)
  (define Asub (top-basic-set A))
  (define Bsub (top-basic-set B))
  (define Csub (basic-intersect Asub Bsub))
  (cond [(different? Csub)  (top-union A B)]
        [(eq? Csub Asub)  A]
        [(eq? Csub Bsub)  B]
        [else  (top-basic Csub)]))

(: bot-bot-tagged-intersect (Bot-Tagged Bot-Tagged -> (U Bot-Tagged Empty-Set)))
(define (bot-bot-tagged-intersect A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (bot-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (bot-tagged a-tag Csub)])]
        [else  empty-set]))

(: bot-top-tagged-intersect (Bot-Tagged Top-Tagged -> (U Bot-Tagged Empty-Set)))
(define (bot-top-tagged-intersect A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (bot-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [else  (bot-tagged a-tag Csub)])]
        [else  A]))

(: top-top-tagged-intersect (Top-Tagged Top-Tagged -> (U Top-Tagged Top-Union)))
(define (top-top-tagged-intersect A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (define Asub (top-tagged-set A))
         (define Bsub (top-tagged-set B))
         (define Csub (set-intersect Asub Bsub))
         (cond [(eq? Csub Asub)  A]
               [(eq? Csub Bsub)  B]
               [else  (top-tagged a-tag Csub)])]
        [else  (top-union A B)]))

;; ===================================================================================================
;; Subset or equal

(: set-subseteq? (Set Set -> Boolean))
(define (set-subseteq? A B)
  (cond [(eq? A B)  #t]
        [(empty-set? A)  #t]
        [(empty-set? B)  #f]
        [(universe? B)   #t]
        [(universe? A)   #f]
        [(bot-set? A)
         (if (bot-set? B)
             (cond [(and (bot-entry? A) (bot-entry? B))  (bot-bot-entry-subseteq? A B)]
                   [else  (bot-bot-subseteq? A B)])
             (cond [(and (bot-entry? A) (top-entry? B))  (bot-top-entry-subseteq? A B)]
                   [else  (bot-top-subseteq? A B)]))]
        [else
         (if (bot-set? B)
             #f
             (cond [(and (top-entry? A) (top-entry? B))  (top-top-entry-subseteq? A B)]
                   [else  (top-top-subseteq? A B)]))]))

(: bot-bot-subseteq? ((U Bot-Entry Bot-Union) (U Bot-Entry Bot-Union) -> Boolean))
(define (bot-bot-subseteq? A B)
  (andmap (λ: ([Asub : Bot-Entry])
            (define Bsub (bot-union-ref B (bot-tag Asub)))
            (if (empty-set? Bsub) #f (bot-bot-entry-subseteq? Asub Bsub)))
          (bot-union-sets A)))

(: bot-top-subseteq? ((U Bot-Entry Bot-Union) (U Top-Entry Top-Union) -> Boolean))
(define (bot-top-subseteq? A B)
  (andmap (λ: ([Asub : Bot-Entry])
            (define Bsub (top-union-ref B (bot-tag Asub)))
            (if (universe? Bsub) #t (bot-top-entry-subseteq? Asub Bsub)))
          (bot-union-sets A)))

(: top-top-subseteq? ((U Top-Entry Top-Union) (U Top-Entry Top-Union) -> Boolean))
(define (top-top-subseteq? A B)
  (andmap (λ: ([Bsub : Top-Entry])
            (define Asub (top-union-ref A (top-tag Bsub)))
            (if (universe? Asub) #f (top-top-entry-subseteq? Asub Bsub)))
          (top-union-sets B)))

(: bot-bot-entry-subseteq? (Bot-Entry Bot-Entry -> Boolean))
(define (bot-bot-entry-subseteq? A B)
  (cond [(and (bot-basic? A)  (bot-basic? B))   (bot-bot-basic-subseteq? A B)]
        [(and (bot-tagged? A) (bot-tagged? B))  (bot-bot-tagged-subseteq? A B)]
        [else  #f]))

(: bot-top-entry-subseteq? (Bot-Entry Top-Entry -> Boolean))
(define (bot-top-entry-subseteq? A B)
  (cond [(and (bot-basic? A)  (top-basic? B))   (bot-top-basic-subseteq? A B)]
        [(and (bot-tagged? A) (top-tagged? B))  (bot-top-tagged-subseteq? A B)]
        [else  #t]))

(: top-top-entry-subseteq? (Top-Entry Top-Entry -> Boolean))
(define (top-top-entry-subseteq? A B)
  (cond [(and (top-basic? A)  (top-basic? B))   (top-top-basic-subseteq? A B)]
        [(and (top-tagged? A) (top-tagged? B))  (top-top-tagged-subseteq? A B)]
        [else  #f]))

(: bot-bot-basic-subseteq? (Bot-Basic Bot-Basic -> Boolean))
(define (bot-bot-basic-subseteq? A B)
  (define res (basic-subseteq? A B))
  (if (different? res) #f res))

(: bot-top-basic-subseteq? (Bot-Basic Top-Basic -> Boolean))
(define (bot-top-basic-subseteq? A B)
  (define res (basic-subseteq? A (top-basic-set B)))
  (if (different? res) #t res))

(: top-top-basic-subseteq? (Top-Basic Top-Basic -> Boolean))
(define (top-top-basic-subseteq? A B)
  (define res (basic-subseteq? (top-basic-set A) (top-basic-set B)))
  (if (different? res) #f res))

(: bot-bot-tagged-subseteq? (Bot-Tagged Bot-Tagged -> Boolean))
(define (bot-bot-tagged-subseteq? A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (bot-tagged-tag B))
         (set-subseteq? (bot-tagged-set A) (bot-tagged-set B))]
        [else  #f]))

(: bot-top-tagged-subseteq? (Bot-Tagged Top-Tagged -> Boolean))
(define (bot-top-tagged-subseteq? A B)
  (define a-tag (bot-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (set-subseteq? (bot-tagged-set A) (top-tagged-set B))]
        [else  #t]))

(: top-top-tagged-subseteq? (Top-Tagged Top-Tagged -> Boolean))
(define (top-top-tagged-subseteq? A B)
  (define a-tag (top-tagged-tag A))
  (cond [(eq? a-tag (top-tagged-tag B))
         (set-subseteq? (top-tagged-set A) (top-tagged-set B))]
        [else  #f]))

;; ===================================================================================================

(define-syntax ssig
  (set-sig
   #'(Nonextremal-Set Universe Empty-Set Value)
   #'set-member?
   #'universe?
   #'empty-set?
   #'universe
   #'empty-set
   #'set-intersect
   #'set-join
   #'set-subseteq?))

(define-syntax rsig
  (set-sig
   #'(Nonextremal-Pair-Set Full-Pair-Set Empty-Pair-Set (Pair Value Value))
   #'pair-set-member?
   #'pairs?
   #'empty-pair-set?
   #'pairs
   #'empty-pair-set
   #'pair-set-intersect
   #'pair-set-join
   #'pair-set-subseteq?))

(define-syntax sig
  (rect-sig
   (syntax-local-value #'rsig)
   (syntax-local-value #'ssig)
   (syntax-local-value #'ssig)
   #'Nonextremal-Pair-Set #'Nonextremal-Pair-Set-fst #'Nonextremal-Pair-Set-snd
   #'Pair #'car #'cdr))

(define-rect-constructor pair-set sig)
(define-rect-ops pair-set sig)
