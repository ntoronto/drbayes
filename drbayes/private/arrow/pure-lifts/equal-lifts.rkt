#lang typed/racket/base

(require racket/promise
         "../../set.rkt"
         "../types.rkt"
         "../cache.rkt")

(provide (all-defined-out))

(: equal?/bot (-> Bot-Arrow))
(define ((equal?/bot) a)
  (cond [(pair? a)  (equal? (car a) (cdr a))]
        [else  (bottom (delay (format "equal?: expected pair; given ~e" a)))]))

(: equal?/pre (-> Pre-Arrow))
(define (equal?/pre)
  (define fun (make-pre-mapping-fun/memo))
  (make-pre-arrow/memo
   (λ (A)
     (define-values (A1 A2) (set-projs A))
     (cond [(or (empty-set? A1) (empty-set? A2))  empty-pre-mapping]
           [else
            (nonempty-pre-mapping
             (cond [(empty-set? (set-intersect A1 A2))  falses]
                   [(and (set-singleton? A1) (set-singleton? A2))  trues]
                   [else  bools])
             (fun (λ (B)
                    (cond [(set-member? B #f)  (values (pair-set A1 A2) #f)]
                          [(set-member? B #t)  (let ([A  (set-intersect A1 A2)])
                                                 (values (set-pair A A)
                                                         (set-singleton? A)))]
                          [else  (values empty-set #t)]))))]))))
