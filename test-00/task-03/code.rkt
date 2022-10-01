#lang racket/base

(require racket/vector)
(provide taskIII)

;; Вычисляет площадь кумлуятивно до тех пор пока не перевали за 1/2
;; или не закончит считать
(define (taskIII t)
  ;; Calculates surface of this tree, adds to total-surface and returns
  ;; calculated value. However if total-surface is more than 1/2 it cc-exits
  ;; with #t
  (define (surface t node-surface total-surface cc-exit)
    (define result
      (if (number? t)
          (+ total-surface (* t node-surface))
          (foldl
           (λ (node s) (surface node (/ node-surface 4) s cc-exit))
           total-surface
           (vector->list t))))
    (if (> result 1/2) (cc-exit #t) result))

  (call/cc
   (λ (cc-exit)
     (> (surface t 1 0 cc-exit) 1/2))))
