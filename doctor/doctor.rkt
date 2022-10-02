; заготовка "Доктора". Сентябрь 2022
#lang scheme/base
(require racket/vector)

; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name)
  )

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
  (newline)
  (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
  (let ((user-response (read)))
    (cond
      	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
                  )
            )
    )
  )

; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply user-response)
  (case (random 0 2) ; с равной вероятностью выбирается один из двух способов построения ответа
    ((0) (hedge-answer))  ; 1й способ
    ((1) (qualifier-answer user-response)) ; 2й способ

    )
  )

; 1й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
  (pick-random-vector '#((please go on)
                         (many people have the same sorts of feelings)
                         (many of my patients have told me the same thing)
                         (please continue)
                         (this is very important)
                         (please tell me more)
                         (people often tell me this))
                      )
  )

; случайный выбор одного из элементов непустого вектора
(define (pick-random-vector vctr)
  (vector-ref vctr (random 0 (vector-length vctr)))
  )

; 2й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату случайно выбранного нового начала
(define (qualifier-answer user-response)
  (append (pick-random-vector '#((you seem to think that)
                                 (you feel that)
                                 (why do you believe that)
                                 (why do you say that)
                                 (what makes you think that)
                                 (what makes you beleive that)
                                 (what made you think that))
                              )
          (change-person user-response)
          )
  )

; замена лица во фразе
(define (change-person phrase)
  (many-replace-v3
   		(build-replacement-pairs
                 '(am are)
                 '(are am)
                 '(i you)
                 '(me you)
                 '(mine yours)
                 '(my your)
                 '(myself yourself)
                 '(you i)
                 '(your my)
                 '(yours mine)
                 '(yourself myself)
                 '(we you)
                 '(us you)
                 '(our your)
                 '(ours yours)
                 '(ourselves yourselves)
                 '(yourselves ourselves)
                 '(shall will))
                phrase)
  )

;; Структура данных содержащая пары для замен местоимений
(define (build-replacement-pairs . pairs) pairs)
(define (has-replacement? what replacement-pairs) (if (not (assoc what replacement-pairs)) #f #t))
(define (get-replacement what replacement-pairs) (cadr (assoc what replacement-pairs)))
(define (replace-or-keep what replacement-pairs)
  (let ([result (assoc what replacement-pairs)])
    (if (not result)
        what
        (cadr result))))

; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
  (cond ((null? lst) lst)
        (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                          (car lst) ; иначе в начале ответа помещается начало списка без изменений
                          )
                      (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                      )
                )
              )
        )
  )

(define (many-replace-v1 replacement-pairs lst)
  (if (null? lst)
      lst
      (let ([word (car lst)] [tail (cdr lst)])
        (if (has-replacement? word replacement-pairs)
            (cons (get-replacement word replacement-pairs) (many-replace-v1 replacement-pairs tail))
            (cons word (many-replace-v1 replacement-pairs tail))))))

(define (many-replace-v2 replacement-pairs lst)
  (reverse
   (let loop ([input lst] [output '()])
     (if (null? input)
         output
         (loop (cdr input) (cons (replace-or-keep (car input) replacement-pairs) output))))))

(define (many-replace-v3 replacement-pairs lst)
  (map
   (λ (word) (let ([result (assoc word replacement-pairs)])
               (if (not result)
                   word
                   (cadr result))))
   lst))

(define (many-replace-v3.5 replacement-pairs lst)
  (map
   replace-or-keep
   lst))

; в Racket нет vector-foldl, реализуем для случая с одним вектором (vect-foldl f init vctr)
; у f три параметра i -- индекс текущего элемента, result -- текущий результат свёртки, elem -- текущий элемент вектора
(define (vector-foldl f init vctr)
  (let ((length (vector-length vctr)))
    (let loop ((i 0) (result init))
      (if (= i length) result
          (loop (add1 i) (f i result (vector-ref vctr i)))))))

; аналогично от конца вектора к началу
(define (vector-foldr f init vctr)
  (let ((length (vector-length vctr)))
    (let loop ((i (sub1 length)) (result init))
      (if (= i -1) result
          (loop (sub1 i) (f i result (vector-ref vctr i)))))))