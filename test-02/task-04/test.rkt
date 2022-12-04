#lang racket/base

(require "code.rkt" rackunit)

(check-equal? (nthbit 1) 1)
(check-equal? (nthbit 2) 1)
(check-equal? (nthbit 3) 0)
(check-equal? (nthbit 4) 1)
(check-equal? (nthbit 5) 0)
(check-equal? (nthbit 6) 0)
(check-equal? (nthbit 7) 0)
(check-equal? (nthbit 8) 1)
(check-equal? (nthbit 16) 1)
(check-equal? (nthbit 32) 1)
(check-equal? (nthbit 64) 1)
(check-equal? (nthbit 127) 0)
(check-equal? (nthbit 128) 1)
(check-equal? (nthbit 129) 0)
