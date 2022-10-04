#lang racket/base

(require "code.rkt" rackunit)

(check-equal? (fun5 #()) #t)
(check-equal? (fun5 #(10 #() #())) #t)
(check-equal? (fun5 #(1 #(-1 #(3 #() #()) #(3 #() #())) #(-2 #() #()))) #f)
(check-equal? (fun5 #(1 #(-1 #(-3 #() #()) #(-3 #() #())) #(-2 #() #()))) #t)
