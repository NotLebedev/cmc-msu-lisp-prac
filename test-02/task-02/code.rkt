#lang racket/base

(provide filter1 filter2 )

(define (filter1 f lst)
  (reverse (foldl (lambda (x y) (if (f x) (cons x y) y)) null lst)))
(define (filter2 f lst)
  (foldr (lambda (x y) (if (f x) (cons x y) y)) null lst))