#lang typed/racket/base

(require racket/list
         "../set/tree-value.rkt"
         "../set/real-set.rkt")

(provide (all-defined-out))

(define-type Interval-Splitter
  (Nonempty-Interval -> (Values (Listof Nonempty-Interval) (Listof Positive-Flonum))))

(define-type Indexes (Listof (U random-index ifte*-index)))
(struct: random-index ([index : Tree-Index] [split : (U #f Interval-Splitter)]) #:transparent)
(struct: ifte*-index ([index : Tree-Index] [true : (Promise Indexes)] [false : (Promise Indexes)])
  #:transparent)

(: intersect-and-filter ((Listof Nonempty-Interval) Nonempty-Interval
                                                    -> (Values (Listof Nonempty-Interval)
                                                               (Listof Positive-Flonum))))
(define (intersect-and-filter Is A)
  (let: loop ([Is Is] [new-Is : (Listof Nonempty-Interval)  empty]
                      [ps : (Listof Positive-Flonum)  empty])
    (cond [(empty? Is)  (values (reverse new-Is) (reverse ps))]
          [else
           (define I (interval-intersect (first Is) A))
           (cond [(empty-real-set? I)  (loop (rest Is) new-Is ps)]
                 [else
                  (define p (interval-measure I))
                  (cond [(p . <= . 0.0)  (loop (rest Is) new-Is ps)]
                        [else  (loop (rest Is) (cons I new-Is) (cons p ps))])])])))

(: make-constant-splitter ((Listof Nonempty-Interval) -> Interval-Splitter))
(define (make-constant-splitter Is)
  (let-values ([(Is _)  (intersect-and-filter Is unit-interval)])
    (λ (A) (intersect-and-filter Is A))))
