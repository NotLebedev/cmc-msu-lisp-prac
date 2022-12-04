#lang racket/base

(provide n!!!!)

(define Y
  (lambda (f) ((lambda (x) (x x))
               (lambda (g) (f (lambda args (apply (g g) args)))))))

(define (n!!!! i) 0)