#lang racket/base

(require "code.rkt" rackunit)

(check-equal? (vector-fold-right (lambda (i val el) (cons el val)) '() #()) '())
(check-equal? (vector-fold-right (lambda (i val el) (cons el val)) '() #(10 20 30)) '(10 20 30))