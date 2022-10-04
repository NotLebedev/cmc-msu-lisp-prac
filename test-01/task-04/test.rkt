#lang racket/base

(require "code.rkt" rackunit)

(check-equal? (fun4 #(1 #(-1 #(3 #() #()) #(3 #() #())) #(-2 #() #())) 2 1) 2)
(check-equal? (fun4 #() 1 10) 0)
(check-equal? (fun4 #(-10 #() #()) 0 0) 1)
