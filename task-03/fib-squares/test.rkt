#lang racket/base

(require rackunit "code.rkt")

(check-equal? (list-fib-squares-a 1) '(0))
(check-equal? (list-fib-squares-a 3) '(0 1 1))
(check-equal? (list-fib-squares-a 5) '(0 1 1 4 9))
(check-equal? (list-fib-squares-a 10) '(0 1 1 4 9 25 64 169 441 1156))