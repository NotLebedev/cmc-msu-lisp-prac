#lang scheme/base

(require math/number-theory)
(provide fun3 )

(define (fun3  n) (foldl * 1 (next-primes 0 n)))