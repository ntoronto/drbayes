#lang typed/racket/base

(require (for-syntax racket/base
                     racket/syntax)
         racket/list
         racket/promise)

(provide (all-defined-out))

(define-type (Maybe-Promise T) (U T (Promise T)))

(define-syntax-rule (maybe-force e)
  (let ([x e])
    (if (promise? x) (force x) x)))

(define-type (Listof+1 A) (Pair A (Listof A)))
(define-type (Listof+2 A) (Pair A (Pair A (Listof A))))

(: map/+2 (All (A B) ((A -> B) (Listof+2 A) -> (Listof+2 B))))
(define (map/+2 f xs)
  (list* (f (first xs))
         (f (second xs))
         (map f (rest (rest xs)))))

(: append/+2 (All (A) ((Listof+2 A) (Listof+2 A) -> (Listof+2 A))))
(define (append/+2 xs ys)
  (list* (first xs) (second xs) (append (rest (rest xs)) ys)))

(: remove-index (All (A) ((Listof A) Integer -> (Listof A))))
(define (remove-index lst i)
  (append (take lst i) (drop lst (+ i 1))))

(: remove-index/+2 (All (A) ((Listof+2 A) Integer -> (Pair A (Listof A)))))
(define (remove-index/+2 lst i)
  (cond [(= i 0)  (cdr lst)]
        [(= i 1)  (cons (first lst) (rest (rest lst)))]
        [else  (list* (first lst) (second lst) (remove-index (rest (rest lst)) (- i 2)))]))

(: list-set (All (A) ((Listof A) Integer A -> (Listof A))))
(define (list-set lst i x)
  (append (take lst i) (cons x (drop lst (+ i 1)))))

(: list-set/+2 (All (A) ((Listof+2 A) Integer A -> (Listof+2 A))))
(define (list-set/+2 lst i x)
  (cond [(= i 0)  (list* x (second lst) (rest (rest lst)))]
        [(= i 1)  (list* (first lst) x (rest (rest lst)))]
        [else  (list* (first lst) (second lst) (list-set (rest (rest lst)) (- i 2) x))]))

(: hasheq2 (All (A B) (A B A B -> (HashTable A B))))
(define (hasheq2 k1 v1 k2 v2) (make-immutable-hasheq (list (cons k1 v1) (cons k2 v2))))

#;;(: weak-value-hash-ref! (All (K V) (-> (HashTable K (Weak-Boxof V)) K (-> V) V)))
(define (weak-value-hash-ref! h k v)
  (define w (hash-ref h k #f))
  (cond [w  (define v* (weak-box-value w))
            (if v* v* (let ([v  (v)])
                        (hash-set! h k (make-weak-box v))
                        v))]
        [else
         (let ([v  (v)])
           (hash-set! h k (make-weak-box v))
           v)]))
