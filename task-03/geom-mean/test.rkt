#lang racket/base

(require rackunit "code.rkt")

(check-equal? (task-03 '((3 4) (2 2 1) (6 8) (4320 3240))) 30.0)
(check-equal? (task-03 '((3 4) (4 5) (1 3) (1 3))) 4.230001127874188)