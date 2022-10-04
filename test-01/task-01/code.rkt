#lang scheme/base

(provide vector-fold-right)

(define (vector-fold-right func init-val vctr)
  (let ([len (vector-length vctr)])
    ;; Loop over all elements of vctr from n-1 to 0
    ;; `i` -- current element index
    ;; `i` -- result of last call of func (or `init-val` on first iteration)
    (let loop ([i (- len 1)] [res init-val])
      (if (= i -1)
          res
          (loop (- i 1) (func i res (vector-ref vctr i)))))))
