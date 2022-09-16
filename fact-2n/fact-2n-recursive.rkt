#lang racket/base

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

(2n!-list-recursive 3)