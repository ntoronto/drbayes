#lang typed/racket

(require math/distributions
         "../private/set/symbol-set.rkt"
         "random-utils.rkt")

(provide (all-defined-out))

(define symbols '(a b c d e f))

(: random-symbol-set (All (A) (-> Symbol-Set)))
(define (random-symbol-set)
  (define n (random 6))
  (define A (apply symbol-set (build-list n (λ (_) (random-element symbols)))))
  (if ((random) . < . 0.5) A (symbol-set-complement A)))

(define moar-symbols '(a b c d e f g h i j))

(: random-symbol (Symbol-Set -> Symbol))
(define (random-symbol A)
  (cond [(empty-symbol-set? A)  (raise-argument-error 'random-symbol "Symbol-Set" A)]
        [(symbol? A)  A]
        [(bot-symbol-set? A)  (random-element (set->list (bot-symbol-set-values A)))]
        [else
         (let loop ()
           (define x (random-element moar-symbols))
           (if (symbol-set-member? A x) x (loop)))]))
