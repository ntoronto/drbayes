#lang typed/racket/base

(require racket/match
         racket/promise
         racket/list
         plot/typed
         "../main.rkt"
         (only-in typed/mred/mred Bitmap%))

(provide (all-defined-out)
         write-bitmap)

#;
(define-type Bitmap%
  (Class (Real Real Boolean)
         ()
         ([get-width (-> Integer)]
          [get-height (-> Integer)]
          [get-argb-pixels
           (case-> 
            (Integer Integer Integer Integer Bytes
                     -> Void)
            (Integer Integer Integer Integer Bytes Boolean
                     -> Void)
            (Integer Integer Integer Integer Bytes Boolean Boolean
                     -> Void))])))

(require/typed
 "untyped-test-utils.rkt"
 [write-bitmap
  (case-> ((Instance Bitmap%) (U Path-String Output-Port) (U 'png 'jpg 'xbm 'xpm 'bmp)
                              -> Boolean)
          ((Instance Bitmap%) (U Path-String Output-Port) (U 'png 'jpg 'xbm 'xpm 'bmp) Natural
                              -> Boolean))]
 )

(: real-set->ivls (Nonempty-Real-Set -> (Listof ivl)))
(define (real-set->ivls I)
  (cond [(interval? I)  (define-values (a b a? b?) (interval-fields I))
                        (list (ivl a b))]
        [else  (append* (map real-set->ivls (interval-list-elements I)))]))

(: maybe-pad-list (All (A) ((Listof A) Integer (-> A) -> (Listof A))))
(define (maybe-pad-list lst n thnk)
  (append lst (build-list (max 0 (- n (length lst))) (λ (_) (thnk)))))

(: cons-product (All (A B) ((Listof A) (Listof B) -> (Listof (Pair A B)))))
(define (cons-product as bs)
  (let a-loop ([as as])
    (cond [(empty? as)  empty]
          [else
           (define a (first as))
           (let b-loop ([bs bs])
             (cond [(empty? bs)  (a-loop (rest as))]
                   [else  (cons (cons a (first bs)) (b-loop (rest bs)))]))])))

(: list-product (All (A) ((Listof (Listof A)) -> (Listof (Listof A)))))
(define (list-product xss)
  (cond [(empty? xss)  (list empty)]
        [else  (cons-product (first xss) (list-product (rest xss)))]))

(: omega-rect->plot-rects (Nonempty-Omega-Set -> (Listof (Listof ivl))))
(define (omega-rect->plot-rects Ω)
  (map (λ: ([lst : (Listof ivl)])
         (maybe-pad-list lst 3 (λ () (ivl 0 1))))
       (list-product (map (λ: ([lst : (Listof ivl)]) (take lst (min (length lst) 3)))
                          (map real-set->ivls (omega-set->list Ω))))))

(: omega->point (Omega -> (Listof Flonum)))
(define (omega->point ω)
  (maybe-pad-list (omega->list ω) 3 random))

(: value->listof-flonum (Maybe-Value -> (Listof Flonum)))
(define (value->listof-flonum v)
  (cond [(flonum? v)  (list v)]
        [(boolean? v)  (list (if v (+ 0.9 (* 0.1 (random))) (* 0.1 (random))))]
        [(pair? v)  (append (value->listof-flonum (car v))
                            (value->listof-flonum (cdr v)))]
        [(null? v)  (list)]
        [(tagged-value? v)  (value->listof-flonum (tagged-value-value v))]
        [else  (list -1.0)]))

