#lang racket/base

(provide 2n!-list 2n!-list-recursive)

;; For integer n > 0 evaluate list of first n even numbers factorials
;; e.i. (2, 24, 720, ... (2n)!), for all other n returns empty list.
(define (2n!-list n)
  ;; Main routine.
  ;; Calculate first `n` fibonacci numbers and return list of
  ;; those on even indices among them in descending order
  (define (fac-even-list n)
    ;; `i` - increasing counter, that is equal to last multiplication done
    ;; `element` - next element to inser in list
    ;; `result` - list accumulating items in reverse order
    ;; `max` - maximum value for counter. Because 0! is skipped it must
    ;;         be equal to n + 2
    (let loop ((i 2) (element 2) (result '()) (max (+ n 2)))
      (if (>= i max)
          result
          (loop (+ i 2) (* element (+ i 2) (+ i 1)) (cons element result) max))))

  (if (and (integer? n) (> n 0))
      ;; To return first `n` even numbers 2 * n numbers total need to be evaluated
      (reverse (fac-even-list (* 2 n)))
      ;; If `n` is non-integer return empty list
      '()))

;; For integer n > 0 evaluate list of first n even numbers factorials
;; e.i. (2, 24, 720, ... (2n)!), for all other n returns empty list.
(define (2n!-list-recursive n)
  (define (fact i)
    (if (= i 2)
        '(2)
        ;; Evaluate list  of values for next even number
        (let ((res (fact (- i 2))))
          ;; Take first element of list from recursive call
          ;; calculate next factorial and add to list
          (cons (* (car res) i (- i 1)) res))))

  (if (and (integer? n) (> n 0))
      ;; To return first `n` even numbers 2 * n numbers total need to be evaluated
      (reverse (fact (* 2 n)))
      ;; If `n` is non-integer return empty list
      '()))