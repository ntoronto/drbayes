#lang racket/base

(require racket/class)

(provide write-bitmap)

(define (write-bitmap bm name kind [quality 100])
  (send bm save-file name kind quality))
