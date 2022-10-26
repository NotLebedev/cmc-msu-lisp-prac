#lang racket/base

(provide right-rot!)

(define-syntax swap
  (syntax-rules ()
    [(swap a b)
     (let ([c b])
       (set! b a)
       (set! a c))]))

(define-syntax right-rot!
  (syntax-rules ()
    ;; Empty sequence and sequence of one element
    ;; do not need to be rotated
    [(right-rot!) (void)]
    [(right-rot! one) (void)]
    ;; Swap last two elements of sequence to
    ;; put last in position and then swap the rest
    [(right-rot! head ... a b)
     (begin
       (swap a b)
       (right-rot! head ... a))]))

;; Написать мутирующее вращение при помощи функции в предлагаемой
;; семантике невозможно, т.к. функции с переменным числом аргументов
;; будет передан список, значением которого будут вычисленные агрументы
;; тогда изменение списка будет менять только его содержимое. Переменные
;; из области видимости вызывающей функции не будут доступны и не поменяются