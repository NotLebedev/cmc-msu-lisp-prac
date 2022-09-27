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
  (define (height-n tree n)
    (and
     (task-4 (tree-left tree) (- n 2))
     (task-4 (tree-right tree) (- n 1))))

  (cond
    [(= h 0) (height-0 tree)]
    [(= h 1) (height-1 tree)]
    [(> h 1) (height-n tree h)]
    [else #f]))
