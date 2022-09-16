#lang racket
(define (norm2 x y z) (+ (expt x 2) (expt y 2) (expt z 2)))
(define (scalar x0 y0 z0 x1 y1 z1) (+ (* x0 x1) (* y0 y1) (* z0 z1)))
(define (colinear? x0 y0 z0 x1 y1 z1) (= 1 (/ (expt (scalar x0 y0 z0 x1 y1 z1) 2)
                                                   (* (norm2 x0 y0 z0) (norm2 x1 y1 z1)))))
(define (codirectional? x0 y0 z0 x1 y1 z1) (and (colinear? x0 y0 z0 x1 y1 z1)
                                                (> (scalar x0 y0 z0 x1 y1 z1) 0)))