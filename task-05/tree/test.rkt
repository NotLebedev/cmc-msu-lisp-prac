#lang racket/base

(require "../../lib/vec-tree.rkt" "code.rkt" rackunit)

(define (flip-tree tree)
  (if (empty-tree? tree)
      tree
      (make-tree
       (tree-data tree)
       (flip-tree (tree-right tree))
       (flip-tree (tree-left tree)))))

; Tests
(define tests (list
               ; r
               ; / \
               ; n n
               ; / \ | \
               ; n n n n
               ; | \ | \
               ; n n n n
               ; \
               ; n
               (task-5 (flip-tree #(1 #(1 #(1 #() #()) #(1 #() #(1 #() #()))) #(1 #(1 #() #(1 #() #())) #(1 #(1 #() #()) #(1 #() #(1 #() #())))))) 5)
               ; r
               ; / \
               ; n n
               ; / \ | \
               ; n n n n
               ; | \ | \
               ; n n n n
               ; \
               ; n
               (task-5 (flip-tree #(1 #(1 #(1 #() #()) #(1 #() #(1 #() #()))) #(1 #(1 #() #(1 #() #())) #(1 #(1 #() #()) #(1 #() #(1 #() #())))))) 4)
               ; r
               ; / \
               ; n n
               ; / \ | \
               ; n n n n
               ; \ | \
               ; n n n
               ; \
               ; n
               (task-5 (flip-tree #(1 #(1 #(1 #() #()) #(1 #() #())) #(1 #(1 #() #(1 #() #())) #(1 #(1 #() #()) #(1 #() #(1 #() #())))))) 5)
               ; r
               ; / \
               ; n n
               ; \ | \
               ; n n n
               ; |
               ; n
               (task-5 (flip-tree #(1 #(1 #() #(1 #() #())) #(1 #(1 #() #()) #(1 #() #(1 #() #()))))) 4)
               ; r
               ; / \
               ; n n
               ; \ | \
               ; n n n
               ; | |
               ; n n
               (task-5 (flip-tree #(1 #(1 #() #(1 #() #())) #(1 #(1 #() #(1 #() #())) #(1 #() #(1 #() #()))))) 4)
               ; r
               ; / \
               ; n n
               ; \
               ; n
               (task-5 (flip-tree #(1 #(1 #() #()) #(1 #() #(1 #() #())))) 3)
               ; r
               ; \
               ; n
               (task-5 (flip-tree #(1 #() #(1 #() #()))) 2)
               ; r
               ; /
               ; n
               (task-5 (flip-tree #(1 #(1 #() #()) #())) 2)
               ; r
               ; / \
               ; n n
               (task-5 (flip-tree #(1 #(1 #() #()) #(1 #() #()))) 2)

               (task-5 (flip-tree #()) 0)
               (task-5 (flip-tree #(1 #() #())) 1)
               (task-5 (flip-tree #()) 1)

               ; r
               ; / \
               ; n n
               ; |
               ; n
               (task-5 (flip-tree #(1 #(1 #(1 #() #()) #()) #(1 #() #()))) 2)

               ; Full tree
               (task-5 #(1 #(1 #(1 #(1 #() #()) #(1 #() #())) #(1 #(1 #() #()) #(1 #() #())))
                           #(1 #(1 #(1 #() #()) #(1 #() #())) #(1 #(1 #() #()) #(1 #() #())))) 4)))

(define answers '(#t #f #f #t #f #t #t #f #f #t #t #f #f #f))

(for-each
 (lambda (x y i)
   (if (equal? x y)
       #t
       (printf "====\ntest: ~a;\nGiven answer: ~a\nCorrect: ~a" i x y)
       )
   )
 tests
 answers
 (build-list (length tests) values)
 )
