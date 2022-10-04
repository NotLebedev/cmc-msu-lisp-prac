#lang racket/base

(require  math/number-theory)
(provide fun2a fun2b)

(define (fun2a n)
  (define (composite? x)
    (and (not (prime? x)) (not (= 1 x))))
  (reverse
   (let filter-prime ([lst (divisors n)])
     (if (null? lst)
         '()
         (let ([num (car lst)])
           (if (composite? num)
               (cons num (filter-prime (cdr lst)))
               (filter-prime (cdr lst))))))))

(define (fun2b n)
  (define (composite? x)
    (and (not (prime? x)) (not (= 1 x))))
  (reverse (filter composite? (divisors n))))