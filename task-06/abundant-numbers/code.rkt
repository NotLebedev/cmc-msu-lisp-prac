#lang racket/base

(require math/number-theory)
(provide sigma-1 even-abundant)

;; Calculate sum of divisors of x
(define (sigma-1 x)
  ;; Takes list of (prime, power) from factorization and
  ;; returns Sum by i from 0 to power (inclusive) of prime^i
  (define (sum-prime-powers lst)
    (let ([prime (car lst)] [power (cadr lst)])
      (foldl + 0 (build-list (+ 1 power) (λ (i) (expt prime i))))))

  ;; For all factors this product will yield exactly sum of all divisors
  ;; because when expression is taken apart all products will
  ;; yield exactly all possible combinations of factors, thus resulting
  ;; in all divisors
  (foldl * 1 (map sum-prime-powers (factorize x))))

;; Find n-th (starting from 0) even abundant number
(define (even-abundant n)
  ;; Loop through all even numbers starting from first even
  ;; abundant number.
  ;; `num` -- current number
  ;; `i` -- index of last found abundant number
  (let loop ([num 12] [i 0])
    (cond
      ;; If number is abundant and n-th number is found
      [(and (> (sigma-1 num) (* 2 num)) (= i n)) num]
      ;; If number is abundant but not n-th abundant increase both number
      ;; and index
      [(> (sigma-1 num) (* 2 num)) (loop (+ num 2) (+ i 1))]
      ;; Otherwise just increase index
      [else (loop (+ num 2) i)])))

;; Hash table with all previous result
(define even-abundant-mem (make-hash '((0 12))))
;; Max result (index and value) in `even-abundant-mem`, because all results below it
;; are known search can resume from this value
(define even-abundant-mem-max '(0 12))
(define (memo-even-abundant x)
  (define (calculate n)
    (let loop ([num (cadr even-abundant-mem-max)] [i (car even-abundant-mem-max)])
      (cond;; If number is abundant and n-th number is found
        [(and (> (sigma-1 num) (* 2 num)) (= i n))
         (set! even-abundant-mem-max (list i num))
         (hash-set! even-abundant-mem i num)
         num]
        ;; If number is abundant but not n-th abundant increase both number
        ;; and index
        [(> (sigma-1 num) (* 2 num))
         ;; No need to set max
         (hash-set! even-abundant-mem i num)
         (loop (+ num 2) (+ i 1))]
        ;; Otherwise just increase index
        [else (loop (+ num 2) i)])))
  (let ([mem (hash-ref even-abundant-mem x #f)])
    (if mem
        mem
        (calculate x))))
