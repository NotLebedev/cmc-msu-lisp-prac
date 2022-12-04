#lang racket/base

(require "code.rkt" rackunit racket/stream)

(define result-list '(1 3 5 9 15 25 27 45 75 81 125 135 225 243 375 405 625 675 729 1125 1215 1875
                        2025 2187 3125 3375 3645 5625 6075 6561 ))

(check-equal? (stream->list (stream-take stream3^m5^n (length result-list))) result-list)