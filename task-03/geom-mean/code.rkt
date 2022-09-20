#lang racket

(provide task-03)

;; For a given list of geometric vetors as lists
;; return geometric mean of their lengths
;;
;; (task-03 (list (list 3 4) (list 2 2 1) (list 6 8) (list 4320 3240))) => 30.0
(define (task-03 list)
  (define (square x) (* x x))

  ;; Norm squred calculated to take root from all
  ;; at once with better precision
  (define (norm2 vec)
    (foldl + 0 (map square vec)))

  ;; Norm2 gives squared length, so root
  ;; must be of 2 * length power
  (expt
   (foldl * 1 (map norm2 list))
   (/ 1 (* 2 (length list)))))