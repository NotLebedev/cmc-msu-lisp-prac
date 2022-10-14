#lang racket/base

(require scheme/mpair)

(provide make-queue front-queue insert-queue! delete-queue! queue? empty-queue?)

(define (get-queue-head q)
  (mcar (mlist-ref q 1)))

(define (get-queue-tail q)
  (mcdr (mlist-ref q 1)))

(define (set-queue-head q e)
  (set-mcar! (mlist-ref q 1) e))

(define (set-queue-tail q e)
  (set-mcdr! (mlist-ref q 1) e))

(define (make-queue)
  (let ([queue-lst (mlist)])
    (mlist 'type$table (mcons queue-lst queue-lst))))

(define (front-queue q)
  (get-queue-head q))

(define (insert-queue! q e)
  '())

(define (delete-queue! q)
  '())

(define (queue? q)
  (equal? (mlist-ref q 0) 'type$table))

(define (empty-queue? q)
  (null? (get-queue-head q)))
