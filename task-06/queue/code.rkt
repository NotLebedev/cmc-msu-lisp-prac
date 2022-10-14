#lang racket/base

(require scheme/mpair)

(provide make-queue front-queue insert-queue! delete-queue! queue? empty-queue?)

(define (make-queue)
  (mlist 'type$table))

(define (front-queue q)
  '())

(define (insert-queue! q e)
  '())

(define (delete-queue! q)
  '())

(define (queue? q)
  (equal? (mlist-ref 0 q) 'type$table))

(define (empty-queue? q)
  '())
