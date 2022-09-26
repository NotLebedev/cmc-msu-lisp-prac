#lang racket/base

(require "../../lib/vec-tree.rkt" "code.rkt" rackunit)

(check-equal? (task-4 #() 0) #t)
(check-equal? (task-4 #(1 #() #()) 1) #t)
(check-equal? (task-4 #() 1) #f)