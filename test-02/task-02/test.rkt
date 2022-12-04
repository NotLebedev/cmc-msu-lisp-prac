#lang racket/base

(require "code.rkt" rackunit)

(check-equal? (filter1 (λ (x) (> x 0)) '(1 -2 3 4 -1 5 0 -17)) (filter (λ (x) (> x 0)) '(1 -2 3 4 -1 5 0 -17)))
(check-equal? (filter2 (λ (x) (> x 0)) '(1 -2 3 4 -1 5 0 -17)) (filter (λ (x) (> x 0)) '(1 -2 3 4 -1 5 0 -17)))
