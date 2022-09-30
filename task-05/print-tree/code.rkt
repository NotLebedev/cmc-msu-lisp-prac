#lang racket/base

(require "../../lib/vec-tree.rkt")

(define (print-tree-by-level-desc tree)
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

  ;; Print elements of `list` with whitespaces in between
  (define (print-list-spaced list)
    (begin
      (print (car list))
      (for-each (λ (element)
                  (begin
                    (display " ")
                    (print element))) (cdr list))))

  ;; Display data from a `level-list` in one line
  (define (print-level level-list)
    (begin
      (print-list-spaced (map (λ (node) (tree-data node)) level-list))
      (display "\n")))


  (when (not (empty-tree? tree))
    ;; `level` next level to display
    ;; `cc` continuation to print top levels (to print in reverse)
    (let f ([level (list tree)] [cc values])
      (if (null? level)
          ;; If level is empty all levels of tree are exhausted and its time to print
          (cc)
          ;; If not evaluate next level and append current to print continuation
          (f (next-level-list level) (λ () (begin (print-level level) (cc))))))))

(print-tree-by-level-desc #())

(display "\n")

(print-tree-by-level-desc #(1 #() #()))

(display "\n")

(print-tree-by-level-desc #(10 #(21 #() #()) #(22 #() #())))

(display "\n")

(print-tree-by-level-desc #(10 #(21 #(31 #() #()) #(32 #() #())) #(22 #() #(34 #() #()))))