#lang racket/base

(require racket/vector racket/list)
(provide fun5)

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

(define (fun5 tree)
  ;; Gets data of tree node or returns `default` if it is an empty node
  (define (data-or tree default)
    (if (empty-tree? tree)
        default
        (tree-data tree)))

  ;; Check that node is a heap and returns height this tree or
  ;; cc-exits with #f if it is not a heap
  (define (check-tree tree cc-exit)
    (if (empty-tree? tree)
        0
        (let* ([left (tree-left tree)]
               [right (tree-right tree)]
               [this-value (tree-data tree)]
               [left-value (data-or left this-value)]
               [right-value (data-or right this-value)])
          ;; First check that data of subtrees are not greater than this node data
          (if (> (max left-value right-value) this-value)
              (cc-exit #f)
              ;; Only if values are okay check subtrees
              (let ([left-height (check-tree left cc-exit)]
                    [right-height (check-tree right cc-exit)])
                ;; When subtrees are okay check if heights are okay
                (if (> (abs (- left-height right-height)) 1)
                    (cc-exit #f)
                    ;; Height of this tree should be one bigger than biggewst of children
                    (+ 1 (max left-height right-height))))))))
  (call/cc
   (λ (cc-exit)
     ;; check-tree will alway exit if tree is not heap so if it ends just
     ;; return #t
     (begin
       (check-tree tree cc-exit)
       #t))))