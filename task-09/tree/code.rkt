#lang racket/base

(require racket/class)
(provide 2tree<%> Empty2tree% Nonempty2Tree%)

(define 2tree<%>
  (interface () isEmpty? printTree))

(define Empty2tree%
  (class* object% (2tree<%>)
    (super-new)

    (define/public (isEmpty?) #t)
    (define/public (printTree) (void))))

(define Nonempty2Tree%
  (class* object% (2tree<%>)
    (init-field tag)
    (init-field data)
    (init-field left)
    (init-field right)

    (super-new)

    (define/public (isEmpty?) #f)
    (define/public (printTree)
      (send left printTree)
      (print tag)
      (send right printTree))
    (define/public (get-tag) tag)
    (define/public (set-tag! x) (set! tag x))
    (define/public (get-data) data)
    (define/public (set-data! x) (set! data x))
    (define/public (get-left) left)
    (define/public (set-left! x) (set! left x))
    (define/public (get-right) left)
    (define/public (set-rgith! x) (set! right x))))