#lang racket/base

(require racket/stream)
(provide stream3^m5^n)

(define ones (stream-cons 1 ones))

(define (streams-add s1 s2)
  (cond ((stream-empty? s1) empty-stream)
        ((stream-empty? s2) empty-stream)
        (else (stream-cons
               (+ (stream-first s1) (stream-first s2))
               (streams-add (stream-rest s1) (stream-rest s2))))))

(define ints
  (stream-cons 1 (streams-add ones ints)))

(define (divisible? n x)
  (zero? (remainder n x)))

(define (may-be-from-3-5 x)
  (or (divisible? x 3) (divisible? x 5)))

(define (3-5-primes-filter str)
  (let ([first (stream-first str)])
    (if (may-be-from-3-5 first)
        (stream-cons
         first
         (3-5-primes-filter (stream-rest str)))
        (3-5-primes-filter (stream-filter
                            (λ (x) (not (divisible? x first)))
                            (stream-rest str))))))

(define stream3^m5^n (stream-cons 1(3-5-primes-filter (stream-rest ints))))