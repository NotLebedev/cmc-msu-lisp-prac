#lang racket/base

(require racket/vector)
(provide taskII)

;; Классическая рекурсия для рекурсивной структуры данных
(define (taskII t s)
  (if (number? t)
      (* t s)
      (foldl + 0 (map (λ (x) (taskII x (/ s 4))) (vector->list t)))))

;; Ради шутки "линейно-итеративное" решение. Строятся последовательно списки вершин для
;; каждого уровня и вычисляется площадь закрашенная на данном уровне.
(define (taskII-linear t s)
  ;; Calculate area of this level for each node
  ;; without regards to the next level (which will be
  ;; handled later). Basically only looks at numbers
  ;; at each node, not looking at deeper nodes.
  (define (q-tree-level-subarea node-list level-surface)
    (define (surface node) (/ (* (vector-count (λ (x) (equal? x 1)) node) level-surface) 4))

    (foldl (λ (node total) (+ total (surface node))) 0 node-list))

  ;; Get all children of node
  (define (node-children node)
    (filter vector? (vector->list node)))

  ;; Flatten list of lists
  (define (list-flatten list)
    (foldl
     ;; Ok because every time result is appended TO a list of four or less elements
     ;; So it takes time proportional of first arg or O(1)
     append
     '()
     (reverse list)))

  ;; Get next level of tree from current level
  (define (q-tree-next-level level-list)
    (list-flatten (map node-children level-list)))

  ;; Check if tree is just a top-level 0 or 1 (full white/ full black)
  (if (number? t)
      (* s t)
      ;; If not run a iterative by-level calculation
      ;; Each iteration evaluates surface area of level and adds it to total,
      ;; list of nodes on next level and surface of one node on next level
      (let loop ([level (list t)] [cur-surface s] [total-surface 0])
        (if (null? level)
            total-surface
            (loop
             (q-tree-next-level level)
             (/ cur-surface 4)
             (+ total-surface (q-tree-level-subarea level cur-surface)))))))
