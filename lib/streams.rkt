#lang racket/base

(require racket/stream)
(provide ones ints streams-add stream-scale)

(define ones (stream-cons 1 ones))

(define (streams-add s1 s2)
  (cond ((stream-empty? s1) empty-stream)
        ((stream-empty? s2) empty-stream)
        (else (stream-cons
               (+ (stream-first s1) (stream-first s2))
               (streams-add (stream-rest s1) (stream-rest s2))))))

(define ints
  (stream-cons 1 (streams-add ones ints)))

(define (stream-scale s f)
  (stream-map (lambda (x) (* x f)) s))