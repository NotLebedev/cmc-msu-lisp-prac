#lang racket/base

(require racket/vector)
(provide taskIV-cc)

;; Цикл пришлось раскрутить, потому что иначе получалось ещё более запутанно
(define (taskIV-cc t s cc)
  (if (number? t)
      (cc (* t s))
      (let ([s-next (/ s 4)])
        (taskIV-cc
         (vector-ref t 0) s-next
         (λ (w)
           (taskIV-cc
            (vector-ref t 1) s-next
            (λ (x)
              (taskIV-cc
               (vector-ref t 2) s-next
               (λ (y)
                 (taskIV-cc
                  (vector-ref t 3) s-next
                  (λ (z) (cc (+ w x y z)))))))))))))
