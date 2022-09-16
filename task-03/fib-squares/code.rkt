#lang racket/base

(provide list-fib-squares-a)

(define (list-fib-squares-a n)
  (define (fib-list n)
    (let loop ((i n) (fib-n-1 1) (fib-n-2 0) (result '()))
      (if (= i 0)
          result
          (loop (- i 1) (+ fib-n-1 fib-n-2) fib-n-1 (cons fib-n-2 result)))))

  (define (square x) (* x x))
  (reverse (map square (fib-list n)))
  )