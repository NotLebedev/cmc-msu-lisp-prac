#lang racket/base

(define (fact3 n)
(let loop ((i n) (result 1))
(if (< i 2) result
(loop (- i 1) (* i result))))
)


(define (2n!-list n)
  ; Append `element` to `list` if `number` is even
  (define (cons-if-even number list element)
    (if (even? number) (cons element list) list))

  ; Calculate first `n` fibonacci numbers and return list of
  ; even indexed among them.
  (define (fac-even-list n)
    (let loop ((i (- n 2)) (result '()) (prev 1) (max n))
      (if (< i 2)
          result
          (loop (- i 1) (cons-if-even (+ i 1) result prev) (* (- max i) prev) n)
   )))

  ; If `n` is odd return empty list
  (if (odd? n) '()
      ; If `n` is even calculate how many numbers yield
      ; `n` even + 0! and 1!
      (reverse (fac-even-list (+ 4 (* n 2)))))
)

(2n!-list 8)