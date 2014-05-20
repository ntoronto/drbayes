#lang typed/racket

(require "../private/set/pair-set.rkt"
         "../private/set/real-set.rkt"
         "../private/set/bool-set.rkt"
         "../private/untyped-utils.rkt"
         "../private/utils.rkt"
         "rackunit-utils.rkt"
         "random-real-set.rkt"
         "random-bool-set.rkt"
         "profile.rkt")

(printf "starting...~n")

(struct: Value ([fst : Flonum] [snd : Boolean]) #:transparent)
(struct: Nonextremal-Rect ([fst : Nonempty-Real-Set] [snd : Nonempty-Bool-Set]) #:transparent)

(define-singleton-type Empty-Rect empty-rect)
(define-singleton-type Full-Rect full-rect)

(define-type Nonfull-Rect (U Nonextremal-Rect Empty-Rect))
(define-type Nonempty-Rect (U Nonextremal-Rect Full-Rect))
(define-type Rect (U Nonextremal-Rect Full-Rect Empty-Rect))

(define-syntax real-set-sig
  (set-sig
   #'(Nonextremal-Real-Set Full-Real-Set Empty-Real-Set Flonum)
   #'real-set-member?
   #'reals?
   #'empty-real-set?
   #'reals
   #'empty-real-set
   #'real-set-intersect
   #'real-set-union
   #'real-set-subseteq?))

(define-syntax bool-set-sig
  (set-sig
   #'(Nonextremal-Bool-Set Full-Bool-Set Empty-Bool-Set Boolean)
   #'bool-set-member?
   #'bools?
   #'empty-bool-set?
   #'bools
   #'empty-bool-set
   #'bool-set-intersect
   #'bool-set-union
   #'bool-set-subseteq?))

(define-syntax self-sig
  (set-sig
   #'(Nonextremal-Rect Full-Rect Empty-Rect Value)
   #'rect-member?
   #'full-rect?
   #'empty-rect?
   #'full-rect
   #'empty-rect
   #'rect-intersect
   #'rect-join
   #'rect-subseteq?))

(define-syntax sig
  (rect-sig
   (syntax-local-value #'self-sig)
   (syntax-local-value #'real-set-sig)
   (syntax-local-value #'bool-set-sig)
   #'Nonextremal-Rect #'Nonextremal-Rect-fst #'Nonextremal-Rect-snd
   #'Value #'Value-fst #'Value-snd))

(define-rect-constructor rect sig)
(define-rect-ops rect sig)

(: rect-equal? (Rect Rect -> Boolean))
(define (rect-equal? A B)
  (and (rect-subseteq? A B) (rect-subseteq? B A)))

(: random-rect (-> Rect))
(define (random-rect)
  (define r (random))
  (cond [(r . < . 0.1)  full-rect]
        [(r . < . 0.2)  empty-rect]
        [else
         (let loop ()
           (define A (rect (random-real-set) (random-bool-set)))
           (if (or (full-rect? A) (empty-rect? A)) (loop) A))]))

(: random-value (Rect -> Value))
(define (random-value A)
  (cond [(empty-rect? A)  (Value +nan.0 #f)]
        [(full-rect? A)   (Value (random-real reals) (random-bool bools))]
        [else
         (match-define (Nonextremal-Rect A1 A2) A)
         (Value (random-real A1) (random-bool A2))]))

(time
 (for: ([_  (in-range 100000)])
   (check-membership-lattice
    empty-rect?
    rect-member?
    rect-subseteq?
    rect-join
    rect-intersect
    random-rect
    random-value)
   (check-bounded-lattice
    rect-equal?
    rect-subseteq?
    rect-join
    rect-intersect
    empty-rect
    full-rect
    random-rect)))
