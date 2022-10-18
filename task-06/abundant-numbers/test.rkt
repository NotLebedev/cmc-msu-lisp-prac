#lang racket/base

(require "code.rkt" rackunit)

(check-equal?
 (map sigma-1 (build-list 20 (λ (x) (+ x 1))))
 '(1 3 4 7 6 12 8 15 13 18 12 28 14 24 24 31 18 39 20 42))


(check-equal?
 (map even-abundant (build-list 20 values))
 '(12 18 20 24 30 36 40 42 48 54 56 60 66 70 72 78 80 84 88 90))

(check-equal?
 (map memo-even-abundant (build-list 20 values))
 '(12 18 20 24 30 36 40 42 48 54 56 60 66 70 72 78 80 84 88 90))

(check-equal?
 (map memo-even-abundant (reverse (build-list 20 values)))
 (reverse '(12 18 20 24 30 36 40 42 48 54 56 60 66 70 72 78 80 84 88 90)))