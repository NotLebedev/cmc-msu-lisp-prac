#lang racket/base

(require rackunit "code.rkt")

(check-equal? (process '((5) (1 2) () (3 4) (2 3) (2 3 4))) '((3 4) (2 3 4)))
(check-equal? (process '((1 1) (0 1) (1 1) (1 2) (5 6) (-10 1))) '((1 1) (1 1) (1 2) (5 6)))
(check-equal? (process '((0) (1 2 3) (4 5 6 -77) (1 -5 0) (5 2 1) (1))) '((1 2 3) (5 2 1) (1)))
(check-equal? (process '(() (1 2 3) (4 5 6 -77) (1 -5 0) (5 2 1) (1))) '((1 2 3) (5 2 1)))