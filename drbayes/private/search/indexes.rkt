#lang typed/racket/base

(require racket/match
         racket/list
         racket/promise
         "../set.rkt"
         "../arrow.rkt"
         "types.rkt"
         "parameters.rkt")

(provide (all-defined-out))

(: annotate-indexes (Indexes -> Ann-Indexes))
(define (annotate-indexes idxs)
  (let loop ([idxs idxs])
    (match idxs
      [(list)  empty]
      [(list (random-index j split) idxs ...)
       (cons (ann-random-index j split 1)
             (loop idxs))]
      [(list (ifte*-index j t f) idxs ...)
       (cons (ann-ifte*-index j (delay (loop (force t))) (delay (loop (force f))))
             (loop idxs))])))

(: expand-ann-ifte*-indexes (-> Ann-Indexes Nonempty-Store-Set Ann-Indexes))
(define (expand-ann-ifte*-indexes idxs S)
  (let loop ([idxs idxs])
    (match idxs
      [(list)  empty]
      [(list (and idx (ann-ifte*-index j t-idxs f-idxs)) idxs ...)
       (define B (store-set-branch-proj S j))
       (cond [(trues? B)   (append (force t-idxs) (loop idxs))]
             [(falses? B)  (append (force f-idxs) (loop idxs))]
             [else  (cons idx (loop idxs))])]
      [(list idx idxs ...)  (cons idx (loop idxs))])))
