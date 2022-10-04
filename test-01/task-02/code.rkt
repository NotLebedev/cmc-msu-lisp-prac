#lang scheme/base

(require  math/number-theory)
(provide fun2a fun2b)

(define (fun2a n)
  ;; Check if number is composite
  ;; (so not prime and not 1 which is not composite)
  (define (composite? x)
    (and (not (prime? x)) (not (= 1 x))))
  (reverse
   ;; Recursive implementation of filter
   ;; `lst` -- list to filter
   ;; returns filtered list
   (let filter-prime ([lst (divisors n)])
     ;; Empty list is already filtered
     (if (null? lst)
         '()
         ;; Otherwise check if number is composite (is filtered through)
         ;; and add or not add it to result of filtering list without it
         (let ([num (car lst)])
           (if (composite? num)
               (cons num (filter-prime (cdr lst)))
               (filter-prime (cdr lst))))))))

(define (fun2b n)
  ;; Check if number is composite
  ;; (so not prime and not 1 which is not composite)
  (define (composite? x)
    (and (not (prime? x)) (not (= 1 x))))

  ;; Reverse is iterative and so is filter.
  ;; Divisors and prime? also can be (and I hope are)
  ;; implemented iteratively so process is iterative
  (reverse (filter composite? (divisors n))))