#lang scheme/base

(require racket/vector racket/list)
(provide fun4)

(define empty-tree #())
(define make-tree vector)
(define (tree-data tree) (vector-ref tree 0))
(define (tree-left tree) (vector-ref tree 1))
(define (tree-right tree) (vector-ref tree 2))
(define (empty-tree? t) (equal? t #()))

(define (fun4 tree r1 r2)
  ;; Cons `what` to `to` if `cond` is #t
  (define (cons-if cond what to)
    (if cond (cons what to) to))

  ;; Create list of all (one, two or zero) children of current node
  (define (node-children node)
    (let ([left (tree-left node)] [right (tree-right node)])
      (cons-if (not (empty-tree? left)) left
               (cons-if (not (empty-tree? right)) right '()))))

  ;; Flatten list of lists
  (define (list-flatten list)
    (foldl
     ;; Ok because every time result is appended TO a list of two or less elements
     ;; So it takes time proportional of first arg or O(1)
     append
     '()
     (reverse list)))

  ;; Create list of nodes on next level. `level-list` is list of nodes
  ;; on current level
  (define (next-level-list level-list)
    (list-flatten (map node-children level-list)))

  (define (count-negative level-list)
    (count (λ (node) (negative? (tree-data node))) level-list))

  (if (empty-tree? tree)
      0
      (let ([low (min r1 r2)] [high (max r1 r2)])
        ;; BFS over nodes of tree. Each iteration gets nodes on next level
        ;; number of levels and running total of valid nodes.
        ;; `level` --list of nodes on current level
        ;; `i` -- current depth
        ;; `cnt` -- running total of valid nodes
        (let loop ([level (list tree)] [i 0] [cnt 0])
          (if (or (null? level) (> i high))
              cnt
              (loop (next-level-list level) (+ i 1) (+ cnt (count-negative level))))))))