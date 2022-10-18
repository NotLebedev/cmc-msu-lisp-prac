#lang racket/base

(require "code.rkt" rackunit)

(define (test1)
  (let ([q (make-queue)])
    (check-pred queue? q))

  (let ([q 13])
    (check-false (queue? q))))


(define (test2)
  (let ([q (make-queue)])
    (insert-queue! q 1)
    (insert-queue! q 2)
    (insert-queue! q 3)
    (insert-queue! q 4)

    (check-pred queue? q)

    (check-false (empty-queue? q))
    (check-equal? (front-queue q) 1)
    (delete-queue! q)

    (check-false (empty-queue? q))
    (check-equal? (front-queue q) 2)
    (delete-queue! q)

    (check-false (empty-queue? q))
    (check-equal? (front-queue q) 3)
    (delete-queue! q)

    (check-false (empty-queue? q))
    (check-equal? (front-queue q) 4)
    (delete-queue! q)

    (check-true (empty-queue? q))))

(define (test3)
  (let ([q (make-queue)])
    (insert-queue! q 1)
    (insert-queue! q 2)

    (check-false (empty-queue? q))
    (check-equal? (front-queue q) 1)
    (delete-queue! q)

    (check-false (empty-queue? q))
    (check-equal? (front-queue q) 2)

    (insert-queue! q 3)

    (check-false (empty-queue? q))
    (check-equal? (front-queue q) 2)

    (delete-queue! q)
    (check-false (empty-queue? q))
    (check-equal? (front-queue q) 3)

    (delete-queue! q)

    (check-true (empty-queue? q))

    (insert-queue! q 4)
    (check-false (empty-queue? q))
    (check-equal? (front-queue q) 4)))

(test1)
(test2)
(test3)