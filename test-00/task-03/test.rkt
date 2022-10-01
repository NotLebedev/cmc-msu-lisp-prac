#lang racket/base

(require "code.rkt" rackunit)

(check-equal? (taskIII #(0 0 1 #(1 #(1 0 1 0) 0 #(1 1 1 0)))) #f)
(check-equal? (taskIII #(#(1 #(1 0 1 0) 0 #(1 1 1 0)) 0 1 #(1 1 0 1))) #t)
(check-equal? (taskIII #(1 0 0 #(1 1 1 0))) #f)
(check-equal? (taskIII #(1 #(0 0 1 0) 0 #(1 1 1 0))) #f)
(check-equal? (taskIII #(1 0 1 1)) #t)
(check-equal? (taskIII 0) #f)
(check-equal? (taskIII 1) #t)