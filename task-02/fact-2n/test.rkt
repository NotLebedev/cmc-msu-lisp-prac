#lang racket/base

(require rackunit "code.rkt")

(test-begin
 (define functions (list 2n!-list 2n!-list-recursive))
 (define cases '([0 ()]
                 [1 (2)]
                 [2 (2 24)]
                 [3 (2 24 720)]
                 [7 (2 24 720 40320 3628800 479001600 87178291200)]
                 [0.5 ()]
                 [-12 ()]))

 (define (validate function case)
   (check-equal? (function (car case)) (cadr case)))

 (define (validate-func function)
   (for-each (lambda (x) (validate function x)) cases))

 (for-each validate-func functions))