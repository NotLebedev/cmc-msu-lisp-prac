#lang racket/base

(require "code.rkt" rackunit)

(check-equal?
 (map n!!!! (build-list 20 (λ (x) (+ x 1))))
 '(1 1 2 3 4 5 12 21 32 45 120 231 384 585 1680 3465 6144 9945 30240 65835))
