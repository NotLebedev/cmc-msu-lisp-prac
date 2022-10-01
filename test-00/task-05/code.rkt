#lang racket/base

(provide taskV)

(define (taskV . funcs)
  (foldl
   (λ (func composition) (λ (x) (func (composition x))))
   values
   (reverse funcs)))
