#lang racket/base

(require scheme/mpair)

(provide make-queue front-queue insert-queue! delete-queue! queue? empty-queue?)

(define (get-queue-head q)
  (mcar (mlist-ref q 1)))

(define (get-queue-tail q)
  (mcdr (mlist-ref q 1)))

(define (set-queue-head! q e)
  (set-mcar! (mlist-ref q 1) e))

(define (set-queue-tail! q e)
  (set-mcdr! (mlist-ref q 1) e))

(define (make-queue)
  (let ([queue-lst '()])
    (mlist 'type$table (mcons queue-lst queue-lst))))

(define (front-queue q)
  (mcar (get-queue-head q)))

(define (insert-queue! q e)
  (let ([head (get-queue-head q)] [tail (get-queue-tail q)])
    (cond
      [(empty-queue? q)
       (let ([queue-lst (mlist e)])
         (set-queue-head! q queue-lst)
         (set-queue-tail! q queue-lst))]
      [else
       (let ([tail-pair (mcons e '())])
         (set-mcdr! tail tail-pair)
         (set-queue-tail! q tail-pair))])))

(define (delete-queue! q)
  (let ([head (get-queue-head q)] [tail (get-queue-tail q)])
    (cond
      ;; Dont do anything on empty queue
      [(empty-queue? q) (void)]
      ;; If queue consists of one element clear it
      [(equal? head tail)
       (let ([queue-lst '()])
         (set-queue-head! q queue-lst)
         (set-queue-tail! q queue-lst))]
      ;; Otherwise set queue head to next element after queue head
      [else (set-queue-head! q (mcdr (get-queue-head q)))])))

(define (queue? q)
  (and
   (mlist? q)
   (equal? (mlist-ref q 0) 'type$table)))

(define (empty-queue? q)
  (null? (get-queue-head q)))
