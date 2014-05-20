#lang typed/racket/base

(provide profile-thunk profile-expr)

(require/typed
 profile
 [profile-thunk  ((-> Any) -> Void)])

(define: b : Boolean #f)

(define-syntax-rule (profile-expr e . args)
  (let* ([thnk  (λ () e)]
         [val  (if b (thnk) #f)])
    (profile-thunk (λ () (set! val (thnk))) . args)
    (assert val (λ: ([x : Any]) x))))
