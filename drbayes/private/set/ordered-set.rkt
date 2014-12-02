#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base
                     racket/syntax)
         racket/match
         "types.rkt"
         "../utils.rkt"
         "../untyped-utils.rkt")

(provide (all-defined-out))

(struct Base-Ordered-Set Base-Bot-Basic () #:transparent)
(define-syntax ordered-set? (make-rename-transformer #'Base-Ordered-Set?))

(define-syntax (define-ordered-set stx)
  (syntax-case stx ()
    [(_ #:names Name name
        #:types Value
        #:predicates value? contained-value? -inf-value? +inf-value?
        #:comparisons = <
        #:guards empty? full? guard
        )
     (with-syntax ([Base-Name-Set    (format-id #'Name "Base-~a-Set" #'Name)]
                   [Base-Name-Set?   (format-id #'Name "Base-~a-Set?" #'Name)]
                   [base-name-set?   (format-id #'name "~a-set?" #'name)]
                   [Full-Name-Set    (format-id #'Name "Full-~a-Set" #'Name)]
                   [full-name-set    (format-id #'name "~as" #'name)]
                   [full-name-set?   (format-id #'name "~as?" #'name)]
                   [Empty-Name-Set   (format-id #'Name "Empty-~a-Set" #'Name)]
                   [empty-name-set   (format-id #'name "empty-~a-set" #'name)]
                   [empty-name-set?  (format-id #'name "empty-~a-set?" #'name)]
                   [Plain-Name-Interval       (format-id #'Name "Plain-~a-Interval" #'Name)]
                   [Plain-Name-Interval?      (format-id #'Name "Plain-~a-Interval?" #'Name)]
                   [Plain-Name-Interval-min   (format-id #'Name "Plain-~a-Interval-min" #'Name)]
                   [Plain-Name-Interval-max   (format-id #'Name "Plain-~a-Interval-max" #'Name)]
                   [Plain-Name-Interval-min?  (format-id #'Name "Plain-~a-Interval-min?" #'Name)]
                   [Plain-Name-Interval-max?  (format-id #'Name "Plain-~a-Interval-max?" #'Name)]
                   [Nonempty-Name-Interval  (format-id #'Name "Nonempty-~a-Interval" #'Name)]
                   [Nonfull-Name-Interval   (format-id #'Name "Nonfull-~a-Interval" #'Name)]
                   [Name-Interval  (format-id #'Name "~a-Interval" #'Name)]
                   [name-interval  (format-id #'name "~a-interval" #'name)]
                   [name-interval-join       (format-id #'name "~a-interval-join" #'name)]
                   [name-interval-intersect  (format-id #'name "~a-interval-intersect" #'name)]
                   [name-interval-subseteq?  (format-id #'name "~a-interval-subseteq?" #'name)]
                   [name-interval-member?  (format-id #'name "~a-interval-member?" #'name)]
                   [Plain-Name-Interval-List
                    (format-id #'Name "Plain-~a-Interval-List" #'Name)]
                   [Plain-Name-Interval-List?
                    (format-id #'Name "Plain-~a-Interval-List?" #'Name)]
                   [Plain-Name-Interval-List-elements
                    (format-id #'Name "Plain-~a-Interval-List-elements" #'Name)]
                   [Plain-Name-Set     (format-id #'Name "Plain-~a-Set" #'Name)]
                   [Nonempty-Name-Set  (format-id #'Name "Nonempty-~a-Set" #'Name)]
                   [Nonfull-Name-Set   (format-id #'Name "Nonfull-~a-Set" #'Name)]
                   [Name-Set           (format-id #'Name "~a-Set" #'Name)]
                   [name-set-join        (format-id #'name "~a-set-join" #'name)]
                   [name-set-intersect   (format-id #'name "~a-set-intersect" #'name)]
                   [name-set-subseteq?   (format-id #'name "~a-set-subseteq?" #'name)]
                   [name-set-member?     (format-id #'name "~a-set-member?" #'name)]
                   [name-set-singleton?  (format-id #'name "~a-set-singleton?" #'name)]
                   [name-set-map         (format-id #'name "~a-set-map" #'name)]
                   )
       #'(begin
           (struct Base-Name-Set Base-Ordered-Set () #:transparent)
           
           (define-syntax base-name-set? (make-rename-transformer #'Base-Name-Set?))
           
           (define-singleton-type Full-Name-Set Base-Name-Set full-name-set)
           (define-singleton-type Empty-Name-Set Base-Name-Set empty-name-set)
           
           ;; ========================================================================================
           ;; Interval type
           
           (struct Plain-Name-Interval Base-Name-Set
             ([min : Value] [max : Value] [min? : Boolean] [max? : Boolean])
             #:transparent)
           
           (define-type Nonempty-Name-Interval (U Plain-Name-Interval Full-Name-Set))
           (define-type Nonfull-Name-Interval (U Plain-Name-Interval Empty-Name-Set))
           (define-type Name-Interval (U Plain-Name-Interval Full-Name-Set Empty-Name-Set))
           
           (: name-interval (-> Value Value Boolean Boolean Name-Interval))
           (define (name-interval a b a? b?)
             (cond [(not (value? a))  (raise-argument-error 'name-interval
                                                            (format "~a" 'Value) 0 a b a? b?)]
                   [(not (value? b))  (raise-argument-error 'name-interval
                                                            (format "~a" 'Value) 1 a b a? b?)]
                   [(or (< b a) (and (= a b) (not (and a? b?))))  empty-name-set]
                   [(and a? b? (-inf-value? a) (+inf-value? b))  full-name-set]
                   [(empty? a b a? b?)  empty-name-set]
                   [(full? a b a? b?)  full-name-set]
                   [else  (let-values ([(a b a? b?)  (guard a b a? b?)])
                            (Plain-Name-Interval a b a? b?))]))
           
           (: name-interval-join
              (case-> (-> Name-Interval Nonempty-Name-Interval Nonempty-Name-Interval)
                      (-> Nonempty-Name-Interval Name-Interval Nonempty-Name-Interval)
                      (-> Name-Interval Name-Interval Name-Interval)))
           (define (name-interval-join I1 I2)
             (cond [(empty-name-set? I1)  I2]
                   [(empty-name-set? I2)  I1]
                   [(eq? I1 I2)  I1]
                   [(full-name-set? I1)  I1]
                   [(full-name-set? I2)  I2]
                   [else
                    (match-define (Plain-Name-Interval a1 b1 a1? b1?) I1)
                    (match-define (Plain-Name-Interval a2 b2 a2? b2?) I2)
                    (define-values (a a?)
                      (cond [(< a1 a2)  (values a1 a1?)]
                            [(< a2 a1)  (values a2 a2?)]
                            [else       (values a1 (or a1? a2?))]))
                    (define-values (b b?)
                      (cond [(< b1 b2)  (values b2 b2?)]
                            [(< b2 b1)  (values b1 b1?)]
                            [else       (values b1 (or b1? b2?))]))
                    (cond [(and (eq? a a1) (eq? b b1) (eq? a? a1?) (eq? b? b1?))  I1]
                          [(and (eq? a a2) (eq? b b2) (eq? a? a2?) (eq? b? b2?))  I2]
                          [else
                           (define I (name-interval a b a? b?))
                           (cond [(empty-name-set? I)
                                  (raise-result-error 'name-interval-join
                                                      (format "~a" 'Nonempty-Name-Interval)
                                                      I)]
                                 [else  I])])]))
           
           (: name-interval-intersect
              (case-> (-> Name-Interval Nonfull-Name-Interval Nonfull-Name-Interval)
                      (-> Nonfull-Name-Interval Name-Interval Nonfull-Name-Interval)
                      (-> Name-Interval Name-Interval Name-Interval)))
           (define (name-interval-intersect I1 I2)
             (cond [(full-name-set? I1)  I2]
                   [(full-name-set? I2)  I1]
                   [(eq? I1 I2)  I1]
                   [(empty-name-set? I1)  I1]
                   [(empty-name-set? I2)  I2]
                   [else
                    (match-define (Plain-Name-Interval a1 b1 a1? b1?) I1)
                    (match-define (Plain-Name-Interval a2 b2 a2? b2?) I2)
                    (define-values (a a?)
                      (cond [(< a2 a1)  (values a1 a1?)]
                            [(< a1 a2)  (values a2 a2?)]
                            [else       (values a1 (and a1? a2?))]))
                    (define-values (b b?)
                      (cond [(< b2 b1)  (values b2 b2?)]
                            [(< b1 b2)  (values b1 b1?)]
                            [else       (values b1 (and b1? b2?))]))
                    (cond [(and (eq? a a1) (eq? b b1) (eq? a? a1?) (eq? b? b1?))  I1]
                          [(and (eq? a a2) (eq? b b2) (eq? a? a2?) (eq? b? b2?))  I2]
                          [else
                           (define I (name-interval a b a? b?))
                           (cond [(full-name-set? I)
                                  (raise-result-error 'name-interval-intersect
                                                      (format "~a" 'Nonfull-Name-Interval)
                                                      I)]
                                 [else  I])])]))
           
           (: name-interval-subseteq? (-> Name-Interval Name-Interval Boolean))
           (define (name-interval-subseteq? I1 I2)
             (cond [(empty-name-set? I1)  #t]
                   [(empty-name-set? I2)  #f]
                   [(eq? I1 I2)           #t]
                   [(full-name-set? I2)   #t]
                   [(full-name-set? I1)   #f]
                   [else
                    (match-define (Plain-Name-Interval a1 b1 a1? b1?) I1)
                    (match-define (Plain-Name-Interval a2 b2 a2? b2?) I2)
                    (and (or (< a2 a1) (and (= a1 a2) (or (not a1?) a2?)))
                         (or (< b1 b2) (and (= b1 b2) (or (not b1?) b2?))))]))
           
           (: name-interval-member? (-> Name-Interval Value Boolean))
           (define (name-interval-member? I x)
             (cond [(not (contained-value? x))  #f]
                   [(empty-name-set? I)  #f]
                   [(full-name-set? I)   #t]
                   [else
                    (match-define (Plain-Name-Interval a b a? b?) I)
                    (cond [(and (< a x) (< x b))  #t]
                          [(< x a)  #f]
                          [(< b x)  #f]
                          [(and (= x a) a?)  #t]
                          [(and (= x b) b?)  #t]
                          [else  #f])]))
           
           ;; ========================================================================================
           ;; Sorted, disjoint interval union type
           
           (struct: Plain-Name-Interval-List Base-Name-Set
             ([elements : (Listof+2 Plain-Name-Interval)])
             #:transparent)
           
           (define-type Plain-Name-Set (U Plain-Name-Interval Plain-Name-Interval-List))
           (define-type Nonfull-Name-Set (U Plain-Name-Set Empty-Name-Set))
           (define-type Nonempty-Name-Set (U Plain-Name-Set Full-Name-Set))
           (define-type Name-Set (U Plain-Name-Set Full-Name-Set Empty-Name-Set))
           
           (: min<? (-> Value Boolean Value Boolean Boolean))
           (define (min<? a1 a1? a2 a2?)
             (or (< a1 a2) (and (= a1 a2) a1? (not a2?))))
           
           (: max<? (-> Value Boolean Value Boolean Boolean))
           (define (max<? b1 b1? b2 b2?)
             (or (< b1 b2) (and (= b1 b2) (not b1?) b2?)))
           
           (: set->list (-> Plain-Name-Set (Listof+1 Plain-Name-Interval)))
           (define (set->list I)
             (if (Plain-Name-Interval? I) (list I) (Plain-Name-Interval-List-elements I)))
           
           (: list->set (case-> (-> (Listof+1 Plain-Name-Interval) Plain-Name-Set)
                                (-> (Listof Plain-Name-Interval) Nonfull-Name-Set)))
           (define (list->set Is)
             (cond [(null? Is)  empty-name-set]
                   [(null? (cdr Is))  (car Is)]
                   [else  (Plain-Name-Interval-List Is)]))
           
           ;; ========================================================================================
           ;; Union
           
           (: interval-list-union (-> (Listof Plain-Name-Interval)
                                      (Listof Plain-Name-Interval)
                                      (U Full-Name-Set (Listof Plain-Name-Interval))))
           (define (interval-list-union I1 I2)
             (cond
               [(null? I1)  I2]
               [(null? I2)  I1]
               [else
                (match-define (Plain-Name-Interval a1 b1 a1? b1?) (car I1))
                (match-define (Plain-Name-Interval a2 b2 a2? b2?) (car I2))
                (cond
                  [(or (< b1 a2) (and (= b1 a2) (not b1?) (not a2?)))
                   ;; ------
                   ;;        ------
                   (let ([I  (interval-list-union (cdr I1) I2)])
                     (if (full-name-set? I) I (cons (car I1) I)))]
                  [(or (< b2 a1) (and (= b2 a1) (not b2?) (not a1?)))
                   ;;        ------
                   ;; ------
                   (let ([I  (interval-list-union I1 (cdr I2))])
                     (if (full-name-set? I) I (cons (car I2) I)))]
                  [(min<? a1 a1? a2 a2?)
                   (cond [(max<? b2 b2? b1 b1?)
                          ;; ------
                          ;;   --
                          (interval-list-union I1 (cdr I2))]
                         [else
                          ;; ------           ------
                          ;;    ------   or      ---
                          (define I (name-interval a1 b2 a1? b2?))
                          (cond [(full-name-set? I)  I]
                                [else  (let ([I  (assert I Plain-Name-Interval?)])
                                         (interval-list-union (cdr I1) (cons I (cdr I2))))])])]
                  [else
                   (cond [(max<? b2 b2? b1 b1?)
                          ;;    ------        ------
                          ;; ------      or   ---
                          (define I (name-interval a2 b1 a2? b1?))
                          (cond [(full-name-set? I)  I]
                                [else  (let ([I  (assert I Plain-Name-Interval?)])
                                         (interval-list-union (cons I (cdr I1)) (cdr I2)))])]
                         [else
                          ;;   --             ---        ---           ------
                          ;; ------   or   ------   or   ------   or   ------
                          (interval-list-union (cdr I1) I2)])])]))
           
           (: name-set-join (case-> (-> Name-Set Nonempty-Name-Set Nonempty-Name-Set)
                                    (-> Nonempty-Name-Set Name-Set Nonempty-Name-Set)
                                    (-> Name-Set Name-Set Name-Set)))
           (define (name-set-join I1 I2)
             (cond [(empty-name-set? I1)  I2]
                   [(empty-name-set? I2)  I1]
                   [(eq? I1 I2)  I1]
                   [(full-name-set? I1)   I1]
                   [(full-name-set? I2)   I2]
                   [else
                    (define I (interval-list-union (set->list I1) (set->list I2)))
                    (if (full-name-set? I) I (list->set (assert I pair?)))]))
           
           ;; ========================================================================================
           ;; Intersection
           
           (: interval-list-intersect (-> (Listof Plain-Name-Interval)
                                          (Listof Plain-Name-Interval)
                                          (Listof Plain-Name-Interval)))
           (define (interval-list-intersect I1 I2)
             (cond
               [(null? I1)  I1]
               [(null? I2)  I2]
               [else
                (match-define (Plain-Name-Interval a1 b1 a1? b1?) (car I1))
                (match-define (Plain-Name-Interval a2 b2 a2? b2?) (car I2))
                (cond
                  [(or (< b1 a2) (and (= b1 a2) (or (not b1?) (not a2?))))
                   ;; ------
                   ;;       ------
                   (interval-list-intersect (cdr I1) I2)]
                  [(or (< b2 a1) (and (= b2 a1) (or (not b2?) (not a1?))))
                   ;;       ------
                   ;; ------
                   (interval-list-intersect I1 (cdr I2))]
                  [(min<? a1 a1? a2 a2?)
                   (cond [(max<? b2 b2? b1 b1?)
                          ;; ------
                          ;;   --
                          (cons (car I2) (interval-list-intersect I1 (cdr I2)))]
                         [else
                          ;; ------           ------
                          ;;    ------   or      ---
                          (define I (name-interval a2 b1 a2? b1?))
                          (cond [(empty-name-set? I)  (interval-list-intersect (cdr I1) I2)]
                                [else  (let ([I  (assert I Plain-Name-Interval?)])
                                         (cons I (interval-list-intersect (cdr I1) I2)))])])]
                  [else
                   (cond [(max<? b2 b2? b1 b1?)
                          ;;    ------        ------
                          ;; ------      or   ---
                          (define I (name-interval a1 b2 a1? b2?))
                          (cond [(empty-name-set? I)  (interval-list-intersect I1 (cdr I2))]
                                [else  (let ([I  (assert I Plain-Name-Interval?)])
                                         (cons I (interval-list-intersect I1 (cdr I2))))])]
                         [else
                          ;;   --             ---        ---           ------
                          ;; ------   or   ------   or   ------   or   ------
                          (cons (car I1) (interval-list-intersect (cdr I1) I2))])])]))
           
           (: name-set-intersect (case-> (-> Name-Set Nonfull-Name-Set Nonfull-Name-Set)
                                         (-> Nonfull-Name-Set Name-Set Nonfull-Name-Set)
                                         (-> Name-Set Name-Set Name-Set)))
           (define (name-set-intersect I1 I2)
             (cond [(full-name-set? I1)  I2]
                   [(full-name-set? I2)  I1]
                   [(eq? I1 I2)  I1]
                   [(empty-name-set? I1)  I1]
                   [(empty-name-set? I2)  I2]
                   [(and (Plain-Name-Interval? I1) (Plain-Name-Interval? I2))
                    (name-interval-intersect I1 I2)]
                   [else
                    (list->set (interval-list-intersect (set->list I1) (set->list I2)))]))
           
           ;; ========================================================================================
           ;; Subseteq
           
           (: interval-list-subseteq? (-> (Listof Plain-Name-Interval)
                                          (Listof Plain-Name-Interval)
                                          Boolean))
           (define (interval-list-subseteq? I1 I2)
             (cond [(null? I1)  #t]
                   [(null? I2)  #f]
                   [else
                    (match-define (Plain-Name-Interval a1 b1 a1? b1?) (car I1))
                    (match-define (Plain-Name-Interval a2 b2 a2? b2?) (car I2))
                    (cond
                      [(or (< b1 a2) (and (= b1 a2) (or (not b1?) (not a2?))))
                       ;; ------
                       ;;       ------
                       #f]
                      [(or (< b2 a1) (and (= b2 a1) (or (not b2?) (not a1?))))
                       ;;       ------
                       ;; ------
                       (interval-list-subseteq? I1 (cdr I2))]
                      [(min<? a1 a1? a2 a2?)
                       ;; ------        ------           ------
                       ;;   --     or      ------   or      ---
                       #f]
                      [(max<? b2 b2? b1 b1?)
                       ;;    ------        ------
                       ;; ------      or   ---
                       #f]
                      [else
                       ;;   --             ---        ---           ------
                       ;; ------   or   ------   or   ------   or   ------
                       (interval-list-subseteq? (cdr I1) I2)])]))
           
           (: name-set-subseteq? (-> Name-Set Name-Set Boolean))
           (define (name-set-subseteq? I1 I2)
             (cond [(empty-name-set? I1)  #t]
                   [(empty-name-set? I2)  #f]
                   [(eq? I1 I2)  #t]
                   [(full-name-set? I2)  #t]
                   [(full-name-set? I1)  #f]
                   [(and (Plain-Name-Interval? I1) (Plain-Name-Interval? I2))
                    (name-interval-subseteq? I1 I2)]
                   [else
                    (interval-list-subseteq? (set->list I1) (set->list I2))]))
           
           ;; ========================================================================================
           ;; Other functions
           
           (: name-set-member? (-> Name-Set Value Boolean))
           (define (name-set-member? I x)
             (cond [(not (contained-value? x))  #f]
                   [(empty-name-set? I)  #f]
                   [(full-name-set? I)   #t]
                   [(Plain-Name-Interval? I)   (name-interval-member? I x)]
                   [else  (ormap (λ: ([I : Plain-Name-Interval]) (name-interval-member? I x))
                                 (Plain-Name-Interval-List-elements I))]))
           
           (: name-set-singleton? (-> Name-Set Boolean))
           (define (name-set-singleton? I)
             (and (Plain-Name-Interval? I)
                  (= (Plain-Name-Interval-min I)
                     (Plain-Name-Interval-max I))))
           
           (: name-set-map (-> (-> Nonempty-Name-Interval Name-Set) Name-Set Name-Set))
           (define (name-set-map f I)
             (cond [(empty-name-set? I)  empty-name-set]
                   [(or (full-name-set? I) (Plain-Name-Interval? I))  (f I)]
                   [else
                    (for/fold ([B : Name-Set  empty-name-set])
                              ([I  (in-list (Plain-Name-Interval-List-elements I))])
                      (name-set-join B (f I)))]))
           ))]))
