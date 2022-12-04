#lang racket/base

(provide when)

(define-syntax when
  (syntax-rules ()
    [(when test) test]
    [(when test exprs ...) (if (when test) (begin exprs ...) #f)]))