#lang typed/racket

(require (for-syntax racket/syntax
                     syntax/parse))

;; ===================================================================================================
;; Function arrow primitives

(: id (All (A) (-> A A)))
(: const (All (A) (-> A (-> Any A))))
(: pair (All (A1 A2) (-> A1 A2 (Pair A1 A2))))
(: fst (All (A1 A2) (-> (Pair A1 A2) A1)))
(: snd (All (A1 A2) (-> (Pair A1 A2) A2)))
(: add (-> (Pair Flonum Flonum) Flonum))

(define (id a) a)
(define (const a) (λ (_) a))
(define fst car)
(define snd cdr)
(define (add xy) (+ (car xy) (cdr xy)))
(define pair cons)

;; ===================================================================================================
;; Random store

(struct Store ([val : (Promise Flonum)] [fst : (Promise Store)] [snd : (Promise Store)])
  #:transparent)

(: store-val (-> Store Flonum))
(: store-fst (-> Store Store))
(: store-snd (-> Store Store))
(define (store-val s) (force (Store-val s)))
(define (store-fst s) (force (Store-fst s)))
(define (store-snd s) (force (Store-snd s)))

(: store->list (-> Store (Listof Flonum)))
(define (store->list s)
  (append (if (promise-forced? (Store-val s)) (list (store-val s)) empty)
          (if (promise-forced? (Store-fst s)) (store->list (store-fst s)) empty)
          (if (promise-forced? (Store-snd s)) (store->list (store-snd s)) empty)))

(: random-store (-> Store))
(define (random-store)
  (Store (delay (random)) (delay (random-store)) (delay (random-store))))

;; ===================================================================================================
;; Bottom arrow

(struct bottom () #:transparent)
(struct (A) just ([value : A]) #:transparent)
(define-type (Maybe A) (U bottom (just A)))

(define-type (Bot-Arrow A B) (-> A (Maybe B)))

(: arr/bot (All (A B) (-> (-> A B) (Bot-Arrow A B))))
(: id/bot (All (A) (Bot-Arrow A A)))
(: const/bot (All (A) (-> A (Bot-Arrow Any A))))
(: fst/bot (All (A1 A2) (Bot-Arrow (Pair A1 A2) A1)))
(: snd/bot (All (A1 A2) (Bot-Arrow (Pair A1 A2) A2)))

(define ((arr/bot f) a) (just (f a)))
(define (id/bot a) ((arr/bot (inst id A)) a))
(define (const/bot a) (arr/bot (const a)))
(define (fst/bot a) ((arr/bot (inst fst A1 A2)) a))
(define (snd/bot a) ((arr/bot (inst snd A1 A2)) a))

(define add/bot (arr/bot add))
(define store-val/bot (arr/bot store-val))
(define store-fst/bot (arr/bot store-fst))
(define store-snd/bot (arr/bot store-snd))

(: >>>/bot (All (A B C) (-> (Bot-Arrow A B) (Bot-Arrow B C) (Bot-Arrow A C))))
(define ((>>>/bot f1 f2) a)
  (let ([b  (f1 a)])
    (if (bottom? b) b (f2 (just-value b)))))

(: &&&/bot (All (A B1 B2) (-> (Bot-Arrow A B1) (Bot-Arrow A B2) (Bot-Arrow A (Pair B1 B2)))))
(define ((&&&/bot f1 f2) a)
  (let ([b1  (f1 a)])
    (if (bottom? b1)
        b1
        (let ([b2  (f2 a)])
          (if (bottom? b2)
              b2
              (just (pair (just-value b1)
                          (just-value b2))))))))

(: first/bot (All (A1 B1 A2) (-> (Bot-Arrow A1 B1) (Bot-Arrow (Pair A1 A2) (Pair B1 A2)))))
(define (first/bot k)
  (&&&/bot (>>>/bot (inst fst/bot A1 A2) k) (inst snd/bot A1 A2)))

;; ===================================================================================================
;; Applicative store arrow

(define-type (Store-Arrow A B) (Bot-Arrow (Pair Store A) B))

(: η/st (All (A B) (-> (Bot-Arrow A B) (Store-Arrow A B))))
(: id/st (All (A) (Store-Arrow A A)))
(: const/st (All (A) (-> A (Store-Arrow Any A))))
(: fst/st (All (A1 A2) (Store-Arrow (Pair A1 A2) A1)))
(: snd/st (All (A1 A2) (Store-Arrow (Pair A1 A2) A2)))

(define ((η/st f) sa) (f (cdr sa)))
(define (id/st sa) ((η/st (inst id/bot A)) sa))
(define (const/st a) (η/st (const/bot a)))
(define (fst/st sa) ((η/st (inst fst/bot A1 A2)) sa))
(define (snd/st sa) ((η/st (inst snd/bot A1 A2)) sa))

(define add/st (η/st add/bot))

(: >>>/st (All (A B C) (-> (Store-Arrow A B) (Store-Arrow B C) (Store-Arrow A C))))
(define (>>>/st k1 k2)
  (>>>/bot (&&&/bot (>>>/bot (inst fst/bot Store A) store-snd/bot)
                    (>>>/bot ((inst first/bot Store Store A) store-fst/bot) k1))
           k2))

(: &&&/st (All (A B1 B2) (-> (Store-Arrow A B1) (Store-Arrow A B2) (Store-Arrow A (Pair B1 B2)))))
(define (&&&/st k1 k2)
  (&&&/bot (>>>/bot ((inst first/bot Store Store A) store-fst/bot) k1)
           (>>>/bot ((inst first/bot Store Store A) store-snd/bot) k2)))

(: let/st (All (A B C) (-> (Store-Arrow A B) (Store-Arrow (Pair B A) C) (Store-Arrow A C))))
(define (let/st expr body)
  (>>>/st (&&&/st expr (inst id/st A)) body))

(define (random/st)
  (>>>/bot (inst fst/bot Store Any) store-val/bot))

;; ===================================================================================================
;; Tests

(require typed/rackunit)

(let ([r  (random-store)])
  (define b ((&&&/st (random/st) (&&&/st (random/st) (const/st null))) (pair r null)))
  (check-equal? (just (store->list r)) b))

(let ([r  (random-store)])
  (define b
    ((let/st (>>>/st (&&&/st (random/st) (random/st)) add/st)
             (let/st (>>>/st (&&&/st (inst fst/st Flonum Any) (random/st)) add/st)
                     (inst fst/st Flonum Any)))
     (pair r null)))
  (check-equal? (just (apply + (store->list r))) b))
