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
         "refinement-search.rkt"
         "refinement-sample.rkt")

(provide (all-defined-out))

(: drbayes-sample-store-sets (Parameterof (Listof Nonempty-Store-Set)))
(define drbayes-sample-store-sets (make-parameter empty))

(: drbayes-sample-stores (Parameterof (Listof Store)))
(define drbayes-sample-stores (make-parameter empty))

(: drbayes-sample (-> meaning Natural (Values (Listof Value) (Listof Nonnegative-Flonum))))
(define (drbayes-sample e n)
  (drbayes-sample-store-sets empty)
  (drbayes-sample-stores empty)
  
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
  
  ;; TODO: worth it to generate samples using `store-set-realize` when S is exact?
  (define S
    (let-values ([(S _)  (refine stores)])
      (if (empty-store-set? S)
          (error 'drbayes-sample "cannot sample from the empty set")
          S)))
  
  (: Ss (Listof Nonempty-Store-Set))
  (: ss (Listof Store))
  (: bs (Listof Value))
  (: ws (Listof Nonnegative-Flonum))
  (define-values (Ss ss bs ws)
    (parameterize ([drbayes-refinement-max-splits  (drbayes-sample-max-splits)]
                   [drbayes-refinement-prob-min  prob-0])
      ;; Build the search
      (define t (build-search-tree refine S idxs))
      
      (let: loop ([i : Natural  0]
                  [Ss : (Listof Nonempty-Store-Set)  empty]
                  [ss : (Listof Store)  empty]
                  [bs : (Listof Value)  empty]
                  [ws : (Listof Nonnegative-Flonum)  empty]
                  [t : Store-Search-Tree  t]
                  [pt : Prob  prob-1])
        (cond
          [(and (i . < . n) (not (prob-0? pt)))
           (let ([i  (+ i 1)])
             (when (= 0 (remainder i 100))
               (printf "i = ~v~n" i)
               (flush-output))
             (let-values ([(p S pt t)  (if refinement-search?
                                           (sample-search-tree pt t)
                                           (values prob-1 S pt t))])
               ;(printf "p = ~v  pt = ~v~n" (prob->flonum p) (prob->flonum pt))
               ;(printf "t = ~a~n" (search-tree-abstract t))
               ;(printf "tree size: ~v~n" (search-tree-size t))
               (cond
                 [(empty-store-set? S)
                  ;(printf "sample-search-tree returned the empty set~n~n")
                  (loop i Ss ss bs ws t pt)]
                 ;; Choose using refinement sampling: density q = numer/denom
                 [refinement-sample?
                  (match (refinement-sample-point refine S idxs)
                    [(store-sample s numer denom)
                     (define b (f (cons s null)))
                     (cond [(bottom? b)
                            ;(printf "b = bottom: ~a~n~n" (force (bottom-message b)))
                            (loop i Ss ss bs ws t pt)]
                           [else
                            ;(printf "success!~n~n")
                            ;; Compute w = 1/p * 1/q
                            (define w (prob-quotient->flonum denom (prob* numer p)))
                            (loop i (cons S Ss) (cons s ss) (cons b bs) (cons w ws) t pt)])]
                    [_
                     ;(printf "refinement-sample-point returned #f~n~n")
                     (loop i Ss ss bs ws t pt)])]
                 ;; Choose uniformly: density q = 1/m
                 [else
                  (define m (store-set-random-measure S))
                  ;(printf "S = ~v~n" (store-set-random-list S))
                  (define s (store-set-realize S))
                  ;(printf "s = ~v~n" (store-random-list s))
                  (define b (f (cons s null)))
                  (cond [(bottom? b)
                         ;(printf "b = bottom: ~a~n~n" (force (bottom-message b)))
                         (loop i Ss ss bs ws t pt)]
                        [else
                         ;(printf "success!~n~n")
                         ;; Compute w = 1/p * 1/q
                         (define w (prob-quotient->flonum m p))
                         (loop i (cons S Ss) (cons s ss) (cons b bs) (cons w ws) t pt)])])))]
          [else
           (when (search-fail? t)
             (error 'drbayes-sample "cannot sample from the empty set"))
           (when (prob-0? pt)
             (error 'drbayes-sample "cannot sample from a zero-probability set"))
           (values Ss ss bs ws)]))))
  
  (drbayes-sample-store-sets Ss)
  (drbayes-sample-stores ss)
  
  (values bs ws))
