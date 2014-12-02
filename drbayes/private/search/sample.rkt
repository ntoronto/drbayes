#lang typed/racket/base

(require racket/match
         racket/promise
         "../set.rkt"
         "../flonum.rkt"
         "../arrow.rkt"
         "../language.rkt"
         "types.rkt"
         "parameters.rkt"
         "search-tree.rkt"
         "indexes.rkt"
         "refinement-search.rkt"
         "refinement-sample.rkt")

(provide (all-defined-out))

(: drbayes-sample (-> meaning Natural (Values (Listof Value) (Listof Flonum))))
(define (drbayes-sample e n)
  (define refinement-search? (drbayes-refinement-search?))
  (define refinement-sample? (drbayes-refinement-sample?))
  
  (when (and (not refinement-sample?)
             (not refinement-search?))
    (printf "Warning: (drbayes-refinement-search?) and (drbayes-refinement-sample?) are both #f.
DrBayes will do rejection sampling, and will not recur through conditionals.~n"))
  
  (define-values (f h idxs)
    (match-let ([(meaning _ f h k)  e])
      (values (run/bot* f) (run/pre* h) (k j0))))
  
  (define refine (make-preimage-refiner h universe))
  
  (define S
    (let ([S  (refine stores)])
      (if (empty-store-set? S)
          (error 'drbayes-sample "cannot sample from the empty set")
          S)))
  
  (parameterize ([drbayes-refinement-axis-prob-min  (drbayes-sample-axis-prob-min)]
                 [drbayes-refinement-prob-min  prob-0])
    (define t
      (build-search-tree refine S (annotate-indexes idxs)))
    
    ;(printf "initial tree size: ~v~n~n" (search-tree-size t))
    
    (let: loop ([i : Natural  0]
                [bs : (Listof Value)   empty]
                [ws : (Listof Nonnegative-Flonum)  empty]
                [t : Store-Search-Tree  t]
                [pt : Prob  prob-1])
      (cond
        [(and (i . < . n) (not (prob-0? pt)))
         (let ([i  (+ i 1)])
           (when (= 0 (remainder i 100))
             (printf "i = ~v~n" i)
             (flush-output))
           (let-values ([(leaf p t pt)  (if refinement-search?
                                            (sample-search-tree t pt)
                                            (values (search-leaf S prob-1) prob-1 t pt))])
             (printf "tree size: ~v~n" (search-tree-size t))
             (match-define (search-leaf S m) leaf)
             (cond
               [(empty-store-set? S)
                (printf "sample-search-tree returned the empty set~n")
                (loop i bs ws t pt)]
               ;; Choose using refinement sampling: density q = numer/denom
               [refinement-sample?
                (match (refinement-sample-point refine S idxs)
                  [(store-sample s numer denom)
                   (define b (f (cons s null)))
                   (cond [(bottom? b)
                          (printf "b = bottom: ~a~n" (force (bottom-message b)))
                          (loop i bs ws t pt)]
                         [else
                          ;(printf "success!~n")
                          ;; Compute w = 1/p * 1/q
                          (define w (prob-quotient->flonum denom (prob* numer p)))
                          (loop i (cons b bs) (cons w ws) t pt)])]
                  [_
                   (printf "refinement-sample-point returned #f~n")
                   (loop i bs ws t pt)])]
               ;; Choose uniformly: density q = 1/m
               [else
                ;(printf "S = ~v~n" (store-set-random-list S))
                (define s (store-set-realize S))
                ;(printf "s = ~v~n" (store-random-list s))
                (define b (f (cons s null)))
                (cond [(bottom? b)
                       ;(printf "b = bottom: ~a~n" (force (bottom-message b)))
                       (loop i bs ws t pt)]
                      [else
                       ;(printf "success!~n")
                       ;; Compute w = 1/p * 1/q
                       (define w (prob-quotient->flonum m p))
                       (loop i (cons b bs) (cons w ws) t pt)])])))]
        [else
         (cond [(and (search-leaf? t) (empty-store-set? (search-leaf-value t)))
                (error 'drbayes-sample "cannot sample from the empty set")]
               [(prob-0? pt)
                (error 'drbayes-sample "cannot sample from a zero-probability set")])
         (values bs ws)]))))
