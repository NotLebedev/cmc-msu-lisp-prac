#lang racket/base

(require racket/stream "../../lib/streams.rkt")

(define (powers x)
  (stream-cons 1 (stream-scale (powers x) x)))

;; Merge two streams sorted in ascending order
;; expects head of `first` to be <= head of `second`
(define (merge-sort first second)
  (stream-cons
   (stream-first first)
   (let* ([first (stream-rest first)]
          [first-head (stream-first first)]
          [second-head (stream-first second)])
     (if (<= first-head second-head)
         (merge-sort first second)
         (merge-sort second first)))))

(define powers-3-5 (merge-sort (powers 3) (stream-rest (powers 5))))