#lang racket/base

(provide n!!!!)

(define Y
  (lambda (f) ((lambda (x) (x x))
               (lambda (g) (f (lambda args (apply (g g) args)))))))

(define n!!!!
  (lambda (i)
    ((Y (lambda (f)
          (lambda (n result)
            (case n
              [(0) result]
              [(1) result]
              [(2) (* 2 result)]
              [(3) (* 3 result)]
              [else (f (- n 4) (* n result))]))))
     i 1)))