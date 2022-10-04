#lang scheme/base

(require "code.rkt" rackunit)

(check-equal?  (fun2a 1) '())
(check-equal?  (fun2a 2) '())
(check-equal? (fun2a 12) '(12 6 4))


(check-equal? (fun2b 1) '())
(check-equal? (fun2b 2) '())
(check-equal? (fun2b 12) '(12 6 4))