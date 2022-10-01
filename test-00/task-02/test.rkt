#lang racket/base

(require "code.rkt" rackunit)

(check-equal? (taskII #(1 0 0 #(1 1 1 0)) 16) 7)
(check-equal? (taskII #(1 #(0 0 1 0) 0 #(1 1 1 0)) 16) 8)
(check-equal? (taskII #(1 0 1 1) 4) 3)
(check-equal? (taskII 0 4) 0)
(check-equal? (taskII 1 4) 4)