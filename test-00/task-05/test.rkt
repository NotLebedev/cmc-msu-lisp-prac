#lang racket/base

(require "code.rkt" rackunit)

(check-equal? ((taskV (λ (x) (cons 1 x))
                      (λ (x) (cons 2 x))
                      (λ (x) (cons 3 x))
                      (λ (x) (cons 4 x))
                      (λ (x) (cons 5 x))) '()) '(1 2 3 4 5))

(check-equal? ((taskV (λ (x) (cons 1 x))) '()) '(1))

(check-equal? ((taskV) '()) '())