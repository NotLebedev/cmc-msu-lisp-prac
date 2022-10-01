#lang racket/base

(require "code.rkt" rackunit)

(check-equal? (taskIV-cc #(1 0 0 #(1 1 1 0)) 16 values) 7)
(check-equal? (taskIV-cc #(1 #(0 0 1 0) 0 #(1 1 1 0)) 16 values) 8)
(check-equal? (taskIV-cc #(1 0 1 1) 4 values) 3)
(check-equal? (taskIV-cc 0 4 values) 0)
(check-equal? (taskIV-cc 1 4 values) 4)