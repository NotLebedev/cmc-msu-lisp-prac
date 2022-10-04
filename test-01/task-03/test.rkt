#lang racket/base

(require "code.rkt" rackunit)

(check-equal? (fun3 0) 1)
(check-equal? (fun3 1) 2)
(check-equal? (fun3 2) 6)
(check-equal? (fun3 3) 30)
(check-equal? (fun3 4) 210)
(check-equal? (fun3 10) 6469693230)
(check-equal? (fun3 20) 557940830126698960967415390)
