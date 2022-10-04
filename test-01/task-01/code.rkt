#lang racket/base

(provide vector-fold-right)

(define (vector-fold-right func init-val vctr)
  (let ([len (vector-length vctr)])
    (let loop ([i (- len 1)] [res init-val])
      (if (= i -1)
          res
          (loop (- i 1) (func i res (vector-ref vctr i)))))))
