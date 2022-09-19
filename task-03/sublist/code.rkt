#lang racket/base

(provide process)

;; Takes list of lists of numbers.
;;
;; Returns sublist of `lst` with elements where sum of
;; all elements is bigger than product of all elements
;; of first item.
(define (process lst)
  ;; Cumulative product of list. Default 1.
  (define (product lst) (foldl * 1 lst))
  ;; Cumulative sum of list. Default 0.
  (define (sum lst) (foldl + 0 lst))

  ;; Value of `product` for first element of `lst`
  (let ([first-product (product (car lst))])
    ;; Return #t if sum of `lst` is more then first-product
    ;; #f otherwise
    (define (sum>first-product? lst) (> (sum lst) first-product))

    (filter sum>first-product? lst)))