#lang typed/racket/base

(require racket/promise
         racket/list
         racket/match)

(provide (all-defined-out))

(define-type Tree-Index (Listof Boolean))

(define j0 empty)

(: left (Tree-Index -> Tree-Index))
(define (left j) (cons #t j))

(: right (Tree-Index -> Tree-Index))
(define (right j) (cons #f j))

;; ===================================================================================================
;; Infinite trees used for program domain values

(struct (V) Indexed-Point ([value : (Promise V)]
                           [left  : (Promise (Indexed-Point V))]
                           [right : (Promise (Indexed-Point V))])
  #:transparent)

(: indexed-point-ref (All (V) (-> (Indexed-Point V) Tree-Index V)))
(define (indexed-point-ref t j)
  (let loop ([t t] [j  (reverse j)])
    (cond [(empty? j)  (force (Indexed-Point-value t))]
          [(first j)  (loop (force (Indexed-Point-left t)) (rest j))]
          [else       (loop (force (Indexed-Point-right t)) (rest j))])))

(: indexed-point->list (All (V) (-> (Indexed-Point V) (Listof V))))
(define (indexed-point->list t)
  (let loop ([t t])
    (match-define (Indexed-Point v l r) t)
    (append (if (promise-forced? v) (list (force v)) empty)
            (if (promise-forced? l) (loop (force l)) empty)
            (if (promise-forced? r) (loop (force r)) empty))))

;; ===================================================================================================
;; Unbounded trees used for program domain rectangles

(: true? (-> Any Boolean : #t))
(define (true? a) (eq? a #t))

(struct (N) Indexed-Rect ([value : (U #t N)]
                          [left  : (U #t (Indexed-Rect N))]
                          [right : (U #t (Indexed-Rect N))])
  #:transparent)

(: indexed-rect (All (N) (-> (U #t N)
                             (U #t (Indexed-Rect N))
                             (U #t (Indexed-Rect N))
                             (U #t (Indexed-Rect N)))))
(define (indexed-rect value left right)
  (if (or value left right)
      (Indexed-Rect value left right)
      #t))

(: indexed-rect-ref (All (N) (-> (U #t (Indexed-Rect N)) Tree-Index (U #t N))))
(define (indexed-rect-ref t j)
  (let loop ([t t] [j  (reverse j)])
    (cond [(true? t)  #t]
          [(empty? j)  (Indexed-Rect-value t)]
          [(first j)  (loop (Indexed-Rect-left t) (rest j))]
          [else       (loop (Indexed-Rect-right t) (rest j))])))

(: indexed-rect-meet (All (N) (-> (-> N N (U #f N))
                                  (Indexed-Rect N)
                                  (Indexed-Rect N)
                                  (U #f (Indexed-Rect N)))))
(define (indexed-rect-meet meet t1 t2)
  (let loop ([t1 t1] [t2 t2])
    (match-define (Indexed-Rect v1 l1 r1) t1)
    (match-define (Indexed-Rect v2 l2 r2) t2)
    (let ([v  (cond [(true? v1)  v2] [(true? v2)  v1] [else  (meet v1 v2)])]
          [l  (cond [(true? l1)  l2] [(true? l2)  l1] [else  (loop l1 l2)])]
          [r  (cond [(true? r1)  r2] [(true? r2)  r1] [else  (loop r1 r2)])])
      (if (or (not v) (not l) (not r))
          #f
          (Indexed-Rect v l r)))))

(: indexed-rect-join (All (N) (-> (-> N N (U #t N))
                                  (U #t (Indexed-Rect N))
                                  (U #t (Indexed-Rect N))
                                  (U #t (Indexed-Rect N)))))
(define (indexed-rect-join join t1 t2)
  (let loop ([t1 t1] [t2 t2])
    (cond [(or (true? t1) (true? t2))  #t]
          [else
           (match-define (Indexed-Rect v1 l1 r1) t1)
           (match-define (Indexed-Rect v2 l2 r2) t2)
           (define v (if (or (true? v1) (true? v2)) #t (join v1 v2)))
           (indexed-rect v (loop l1 l2) (loop r1 r2))])))

(: indexed-rect-member? (All (N V) (-> (-> N V Boolean)
                                       (U #t (Indexed-Rect N))
                                       (Indexed-Point V)
                                       Boolean)))
(define (indexed-rect-member? member? t1 t2)
  (let loop ([t1 t1] [t2 t2])
    (cond [(true? t1)  #t]
          [else
           (match-define (Indexed-Rect v1 l1 r1) t1)
           (match-define (Indexed-Point v2 l2 r2) t2)
           (and (if (true? v1) #t (member? v1 (force v2)))
                (loop l1 (force l2))
                (loop r1 (force r2)))])))

(: indexed-rect-subseteq? (All (N) (-> (-> N N Boolean)
                                       (U #t (Indexed-Rect N))
                                       (U #t (Indexed-Rect N))
                                       Boolean)))
(define (indexed-rect-subseteq? subseteq? t1 t2)
  (let loop ([t1 t1] [t2 t2])
    (cond [(true? t2)  #t]
          [(true? t1)  #f]
          [else
           (match-define (Indexed-Rect v1 l1 r1) t1)
           (match-define (Indexed-Rect v2 l2 r2) t2)
           (and (cond [(true? v2)  #t] [(true? v1)  #f] [else  (subseteq? v1 v2)])
                (loop l1 l2)
                (loop r1 r2))])))

(: indexed-rect-unproj (All (N E) (-> (-> N N (U #f N))
                                      (U #t (Indexed-Rect N))
                                      Tree-Index
                                      N
                                      (U #f (Indexed-Rect N)))))
(define (indexed-rect-unproj meet t j B)
  (let loop ([t t] [j  (reverse j)])
    (cond [(true? t)  (loop (Indexed-Rect #t #t #t) j)]
          [(empty? j)
           (match-define (Indexed-Rect v l r) t)
           (let ([v  (if (true? v) B (meet v B))])
             (if v (Indexed-Rect v l r) #f))]
          [else
           (match-define (Indexed-Rect v l r) t)
           (if (first j)
               (let ([l  (loop l (rest j))])
                 (if l (Indexed-Rect v l r) #f))
               (let ([r  (loop r (rest j))])
                 (if r (Indexed-Rect v l r) #f)))])))

(: indexed-rect-sample-point (All (N V) (-> (-> V)
                                            (-> N V)
                                            (U #t (Indexed-Rect N))
                                            (Indexed-Point V))))
(define (indexed-rect-sample-point random-point sample-point t)
  (let loop ([t t])
    (cond [(true? t)  (loop (Indexed-Rect #t #t #t))]
          [else
           (match-define (Indexed-Rect v l r) t)
           (Indexed-Point (delay (if (true? v) (random-point) (sample-point v)))
                          (delay (loop l))
                          (delay (loop r)))])))

(: indexed-rect-measure (All (N) (-> (-> N Flonum) (U #t (Indexed-Rect N)) Flonum)))
(define (indexed-rect-measure measure t)
  (let loop ([t t])
    (cond [(true? t)  1.0]
          [else
           (match-define (Indexed-Rect v l r) t)
           (* (if (true? v) 1.0 (measure v))
              (* (loop l) (loop r)))])))

(: indexed-rect->list (All (N) (-> (U #t (Indexed-Rect N)) (Listof N))))
(define (indexed-rect->list t)
  (let loop ([t t])
    (cond [(true? t)  empty]
          [else
           (match-define (Indexed-Rect v l r) t)
           (append (if (true? v) empty (list v)) (loop l) (loop r))])))
