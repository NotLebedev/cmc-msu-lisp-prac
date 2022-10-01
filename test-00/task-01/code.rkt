#lang racket/base

(provide taskI)

;; В принципе посетить все элементы списка меньше чем за один проход нельзя.
;; Это решение эксплуатриует ровно один проход.
;; В некотором роде оно наилучшее

;; Tricky clever soultion. Accumulator of fold has a pair in the beginning of list
;; storing current minimum and current index. Everything is done in one go as if it
;; was a typical cycle.
(define (taskI lst)
  ;; Checks `element` vs the first element
  ;; of `accum` (representing current minimum and index) if equal
  ;; adds index if less clears list and adds index if more does nothing
  (define (min-idx element accum)
    (let ([cur-min (caar accum)] [cur-idx (cdar accum)] [cur-idx-list (cdr accum)])
      (cond
        ;; Append here appends to a list of length 2 so its ok
        [(= cur-min element) (append (list (cons cur-min (+ 1 cur-idx)) cur-idx) cur-idx-list)]
        [(< cur-min element) (cons (cons cur-min (+ 1 cur-idx)) cur-idx-list)]
        [(> cur-min element) (cons (cons element (+ 1 cur-idx)) (list cur-idx))])))

  (if (null? lst)
      '()
      ;; cdr to remove first element that is used as tracker of current minimum and index
      (cdr (foldl min-idx (list (cons (car lst) 0)) lst))))
