#lang racket/base

(provide list-fib-squares-a fib-list-clever)

;; Returns a list of squares of first n fibonacci
;; numbers in ascending order
(define (list-fib-squares-a n)
  ;; Returns list of first n fibonacci numbers in
  ;; descending order
  (define (fib-list n)
    (let loop ((i n) (fib-n-1 1) (fib-n-2 0) (result '()))
      (if (= i 0)
          result
          (loop (- i 1) (+ fib-n-1 fib-n-2) fib-n-1 (cons fib-n-2 result)))))

  (define (square x) (* x x))
  (reverse (map square (fib-list n))))

(define (fib-list-clever n)
  ;; This fold uses presence of first parameter as counter
  ;; but actual values are ignored
  (define (fib-fold _ result)
    ;; On each iteration take first and second numbers (list comes out
    ;; reversed) and add them and prepend to list
    (cons (+ (car result) (cadr result)) result))

  ;; Returns list of first n fibonacci numbers in
  ;; descending order
  (define (fib-list n)
    (case n
      ;; Because base of fold already has two elements
      ;; it can not correctly output only for n > 2
      [(0) '()]
      [(1) '(0)]
      ;; Each step of fold adds one new value to list, so n - 2
      ;; new values are required
      [else (foldl fib-fold '(1 0) (build-list (- n 2) values))]))

  (define (square x) (* x x))
  (reverse (map square (fib-list n))))