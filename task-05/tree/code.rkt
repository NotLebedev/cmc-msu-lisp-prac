#lang racket/base

(require "../../lib/vec-tree.rkt")
(provide task-4)

;; Checks if tree is a fibbonacci tree of height h
(define (task-4 tree h)
  (define height-0 empty-tree?)
  (define (height-1 tree)
    (and
     (not (empty-tree? tree))
     (empty-tree? (tree-left tree))
     (empty-tree? (tree-right tree))))
  (define (height-n tree n cc-exit)
    (and
     (run (tree-left tree) (- n 1) cc-exit)
     (run (tree-right tree) (- n 2) cc-exit)))

  (define (run tree h cc-exit)
    (if (cond
          [(= h 0) (height-0 tree)]
          [(= h 1) (height-1 tree)]
          [(> h 1) (height-n tree h cc-exit)]
          [else #f])
        #t
        (cc-exit #f)))


  (call/cc (λ (cc-exit) (run tree h cc-exit))))
