#lang racket/base

(require "code.rkt" rackunit)

(check-equal? (taskI (list -1 0 1 -1 0 1 -1)) '(6 3 0))
(check-equal? (taskI (list 5 4 7 3 4 3 12 3)) '(7 5 3))
(check-equal? (taskI (list 6 5 4 3 2 1)) '(5))
(check-equal? (taskI (list 5 5 5 4 4 4 3 3 3)) '(8 7 6))
(check-equal? (taskI '()) '())