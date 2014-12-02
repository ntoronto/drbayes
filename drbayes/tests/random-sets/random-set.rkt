#lang typed/racket

(require drbayes/private/set
         "random-real-set.rkt"
         "random-prob-set.rkt"
         "random-bool-set.rkt"
         "random-store-set.rkt"
         "../test-utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Random Set generation

(: random-bot-real (-> Nonempty-Real-Set))
(define (random-bot-real)
  (define A (random-real-set))
  (if (empty-real-set? A) (random-bot-real) A))

(: random-bot-prob (-> Nonempty-Prob-Set))
(define (random-bot-prob)
  (define A (random-prob-set))
  (if (empty-prob-set? A) (random-bot-prob) A))

(: random-bot-bool (-> Nonempty-Bool-Set))
(define (random-bot-bool)
  (define r (random))
  (cond [(r . < . #i1/3)  trues]
        [(r . < . #i2/3)  falses]
        [else             bools]))

(: random-bot-pair (-> Nonempty-Pair-Set))
(define (random-bot-pair)
  (define A (set-pair (random-set) (random-set)))
  (if (empty-set? A) (random-bot-pair) A))

(: random-bot-store (-> Nonempty-Store-Set))
(define random-bot-store random-nonempty-store-set)

(: random-bot-basic (-> Bot-Basic))
(define (random-bot-basic)
  (define r (random))
  (cond [(r . < . #i1/6)  (random-bot-real)]
        [(r . < . #i2/6)  (random-bot-prob)]
        [(r . < . #i3/6)  (random-bot-bool)]
        [(r . < . #i4/6)  nulls]
        [(r . < . #i5/6)  (random-bot-pair)]
        [else             (random-bot-store)]))

(: random-top-real (-> Top-Basic))
(define (random-top-real)
  (define A (random-real-set))
  (if (reals? A) (random-top-real) (top-basic A)))

(: random-top-prob (-> Top-Basic))
(define (random-top-prob)
  (define A (random-prob-set))
  (if (probs? A) (random-top-prob) (top-basic A)))

(: random-top-bool (-> Top-Basic))
(define (random-top-bool)
  (define r (random))
  (cond [(r . < . #i1/3)  (Top-Basic trues)]
        [(r . < . #i2/3)  (Top-Basic falses)]
        [else             (Top-Basic empty-bool-set)]))

(: random-top-pair (-> Top-Basic))
(define (random-top-pair)
  (define A (pair-set (random-set) (random-set)))
  (if (pairs? A) (random-top-pair) (Top-Basic A)))

(: random-top-store (-> Top-Basic))
(define (random-top-store)
  (define A (random-store-set))
  (if (stores? A) (random-top-store) (Top-Basic A)))

(: random-top-basic (-> Top-Basic))
(define (random-top-basic)
  (define r (random))
  (cond [(r . < . #i1/6)  (random-top-real)]
        [(r . < . #i1/6)  (random-top-prob)]
        [(r . < . #i3/6)  (random-top-bool)]
        [(r . < . #i4/6)  not-nulls]
        [(r . < . #i5/6)  (random-top-pair)]
        [else             (random-top-store)]))

(define a-tag (make-set-tag 'a))
(define b-tag (make-set-tag 'b))

(: random-bot-tagged (-> Bot-Tagged))
(define (random-bot-tagged)
  (define A (random-set))
  (cond [(empty-set? A)  (random-bot-tagged)]
        [((random) . < . 0.5)  (Bot-Tagged a-tag A)]
        [else                  (Bot-Tagged b-tag A)]))

(: random-top-tagged (-> Top-Tagged))
(define (random-top-tagged)
  (define A (random-set))
  (cond [(universe? A)  (random-top-tagged)]
        [((random) . < . 0.5)  (Top-Tagged a-tag A)]
        [else                  (Top-Tagged b-tag A)]))

(define p #i1/6)

(: random-bot-union (-> Bot-Union))
(define (random-bot-union)
  (define As
    (append*
     (list (if ((random) . < . p) (list (random-bot-real)) empty)
           (if ((random) . < . p) (list (random-bot-prob)) empty)
           (if ((random) . < . p) (list (random-bot-bool)) empty)
           (if ((random) . < . p) (list nulls) empty)
           (if ((random) . < . p) (list (random-bot-pair)) empty)
           (if ((random) . < . p) (list (random-bot-store)) empty)
           (if ((random) . < . p) (list (Bot-Tagged a-tag (random-nonempty-set))) empty)
           (if ((random) . < . p) (list (Bot-Tagged b-tag (random-nonempty-set))) empty))))
  (if (or (empty? As) (empty? (rest As)))
      (random-bot-union)
      (apply bot-union As)))

(: random-top-union (-> Top-Union))
(define (random-top-union)
  (define As
    (append*
     (list (if ((random) . < . p) (list (random-top-real)) empty)
           (if ((random) . < . p) (list (random-top-prob)) empty)
           (if ((random) . < . p) (list (random-top-bool)) empty)
           (if ((random) . < . p) (list not-nulls) empty)
           (if ((random) . < . p) (list (random-top-pair)) empty)
           (if ((random) . < . p) (list (random-top-store)) empty)
           (if ((random) . < . p) (list (Top-Tagged a-tag (random-nonfull-set))) empty)
           (if ((random) . < . p) (list (Top-Tagged b-tag (random-nonfull-set))) empty))))
  (if (or (empty? As) (empty? (rest As)))
      (random-top-union)
      (apply top-union As)))

(: set-depth (Parameterof Natural))
(define set-depth (make-parameter 0))

(: random-set (-> Set))
(define (random-set)
  (cond [((set-depth) . <= . 3)
         (parameterize ([set-depth  (+ 1 (set-depth))])
           (define r (random))
           (cond [(r . < . #i1/8)  (random-bot-basic)]
                 [(r . < . #i2/8)  (random-top-basic)]
                 [(r . < . #i3/8)  (random-bot-tagged)]
                 [(r . < . #i4/8)  (random-top-tagged)]
                 [(r . < . #i5/8)  (random-bot-union)]
                 [(r . < . #i6/8)  (random-top-union)]
                 [(r . < . #i7/8)  empty-set]
                 [else             universe]))]
        [((random) . < . 0.5)  empty-set]
        [else                  universe]))

(: random-nonempty-set (-> Nonempty-Set))
(define (random-nonempty-set)
  (define A (random-set))
  (if (empty-set? A) (random-nonempty-set) A))

(: random-nonfull-set (-> Nonfull-Set))
(define (random-nonfull-set)
  (define A (random-set))
  (if (universe? A) (random-nonfull-set) A))

;; ===================================================================================================
;; Random Value generation

(: random-basic-member (Nonempty-Basic -> Value))
(define (random-basic-member A)
  (cond [(real-set? A)  (random-real A)]
        [(prob-set? A)  (random-prob A)]
        [(bool-set? A)  (random-bool A)]
        [(null-set? A)  null]
        [(pair-set? A)  (define-values (A1 A2) (pair-set-projs A))
                        (cons (random-set-member A1) (random-set-member A2))]
        [(store-set? A)  (random-store A)]))

(: random-bot-basic-member (Bot-Basic -> Value))
(define (random-bot-basic-member A)
  (random-basic-member A))

(: random-bot-tagged-member (Bot-Tagged -> Value))
(define (random-bot-tagged-member A)
  (match-define (Bot-Tagged tag Asub) A)
  (tagged-value tag (random-set-member Asub)))

(: random-bot-union-member (Bot-Union -> Value))
(define (random-bot-union-member A)
  (define As (bot-union-sets A))
  (random-set-member (random-element As)))

(define tags (list real-tag bool-tag null-tag pair-tag a-tag b-tag))

(: random-top-basic-member (Top-Basic -> Value))
(define (random-top-basic-member A)
  (let ([A  (top-basic-set A)])
    (define t (basic-tag A))
    (let loop ()
      (define tag (random-element tags))
      (cond [(eq? tag t)  (if (empty-basic? A) (loop) (random-basic-member A))]
            [(eq? tag a-tag)  (tagged-value a-tag (random-universe-member))]
            [(eq? tag b-tag)  (tagged-value b-tag (random-universe-member))]
            [(eq? tag real-tag)  (random-real reals)]
            [(eq? tag prob-tag)  (random-prob probs)]
            [(eq? tag bool-tag)  (if ((random) . < . 0.5) #t #f)]
            [(eq? tag null-tag)  null]
            [(eq? tag pair-tag)  (random-set-member pairs)]
            [(eq? tag store-tag)  (random-set-member stores)]
            [else  (loop)]))))

(: random-top-tagged-member (Top-Tagged -> Value))
(define (random-top-tagged-member A)
  (match-let ([(Top-Tagged t A)  A])
    (let loop ()
      (define tag (random-element tags))
      (cond [(eq? tag t)  (if (empty-set? A) (loop) (random-bot-tagged-member (bot-tagged t A)))]
            [(eq? tag a-tag)  (tagged-value a-tag (random-universe-member))]
            [(eq? tag b-tag)  (tagged-value b-tag (random-universe-member))]
            [(eq? tag real-tag)  (random-real reals)]
            [(eq? tag prob-tag)  (random-prob probs)]
            [(eq? tag bool-tag)  (if ((random) . < . 0.5) #t #f)]
            [(eq? tag null-tag)  null]
            [(eq? tag pair-tag)  (random-set-member pairs)]
            [(eq? tag store-tag)  (random-set-member stores)]
            [else  (loop)]))))

(: random-top-union-member (Top-Union -> Value))
(define (random-top-union-member A)
  (define x (random-universe-member))
  (if (set-member? A x) x (random-top-union-member A)))

(: random-universe-member (-> Value))
(define (random-universe-member)
  (define A (random-set))
  (if (or (empty-set? A) (universe? A)) (random-universe-member) (random-set-member A)))

(: random-set-member (Set -> Value))
(define (random-set-member A)
  (cond [(empty-set? A)   (raise-argument-error 'random-set-member "Nonempty-Set" A)]
        [(universe? A)    (random-universe-member)]
        [(bot-basic? A)   (random-bot-basic-member A)]
        [(bot-tagged? A)  (random-bot-tagged-member A)]
        [(bot-union? A)   (random-bot-union-member A)]
        [(top-basic? A)   (random-top-basic-member A)]
        [(top-tagged? A)  (random-top-tagged-member A)]
        [(top-union? A)   (random-top-union-member A)]))
