#lang info

(define collection 'multi)

(define deps '("base"
               "typed-racket-lib"
               "typed-racket-more"
               "math-lib"
               ))

(define build-deps '("images-lib"
                     "plot-gui-lib"
                     "plot-lib"
                     "profile-lib"
                     ))

(define pkg-desc "DrBayes: A Probabilistic Programming Language")
(define pkg-authors '(ntoronto))

