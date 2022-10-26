#lang racket/base

(require "code.rkt" rackunit)

(let ((x 1) (y 2) (z 3))
  (begin
    (right-rot! x y z)
    (check-equal? (list x y z) '(3 1 2))))

(let ((x 1) (y 2) (z 3) (k 4) (l 5))
  (begin
    (right-rot! x y z k l)
    (check-equal? (list x y z k l) '(5 1 2 3 4))))


(let ((x 1) (y 2) (z 3))
  (begin
    (right-rot! x y)
    (check-equal? (list x y z) '(2 1 3))))

(let ((x 1) (y 2) (z 3))
  (begin
    (right-rot! x)
    (check-equal? (list x y z) '(1 2 3))))

(let ((x 1) (y 2) (z 3))
  (begin
    (right-rot!)
    (check-equal? (list x y z) '(1 2 3))))