#lang racket/base

(require "code.rkt" rackunit racket/class)

(define root
  (new Nonempty2Tree%
       [tag 'root]
       [data "root"]
       [left (new Nonempty2Tree%
                  [tag 'l]
                  [data "a"]
                  [left (new Empty2tree%)]
                  [right (new Nonempty2Tree%
                              [tag 'lr]
                              [data "b"]
                              [left (new Empty2tree%)]
                              [right (new Empty2tree%)])])]
       [right (new Nonempty2Tree%
                   [tag 'r]
                   [data "c"]
                   [left (new Nonempty2Tree%
                              [tag 'rl]
                              [data "d"]
                              [left (new Empty2tree%)]
                              [right (new Nonempty2Tree%
                                          [tag 'rlr]
                                          [data "e"]
                                          [left (new Empty2tree%)]
                                          [right (new Empty2tree%)])])]
                   [right (new Nonempty2Tree%
                               [tag 'rr]
                               [data "f"]
                               [left (new Empty2tree%)]
                               [right (new Empty2tree%)])])]))

(send root printTree)
(newline)
(send (send root get-left) printTree)
(newline)
(send (send root get-left)set-left! (new Nonempty2Tree%
                                         [tag 'll]
                                         [data "T"]
                                         [left (new Empty2tree%)]
                                         [right (new Empty2tree%)]))
(send (send root get-left) printTree)
(newline)
(print (send (send (send root get-left) get-left) isEmpty?))
(newline)
(print (send (send (send root get-left) get-left) get-data))
(newline)