; заготовка "Доктора". Сентябрь 2022
#lang scheme/base
(require racket/vector)

; В учебных целях используется базовая версия Scheme

(define (visit-doctor-v2 stop-word patients-cnt)
  (call/cc
   (λ (cc-exit)
     (let loop ([idx 0])
       (when (= idx patients-cnt) (cc-exit))
       (print '(next!))
       (display "\n")
       (print '(who are you?))
       (display "\n**")
       (let ([name (car (read))])
         (if (equal? name stop-word)
             (begin
               (print '(time to go home))
               (cc-exit))
             (begin
               (visit-doctor name)
               (loop (+ idx 1)))))))))

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента
(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v2 name #())
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

;; Цикл диалога с Доктора с пацинетом
;; `name` -- имя пациента
;; `history` -- история диалога с пациентом
(define (doctor-driver-loop-v2 name history)
  (newline)
  (print '**)
  (let ([user-response (read)])
    (cond
      [(equal? user-response '(goodbye))
       (printf "Goodbye, ~a!\n" name)
       (print '(see you next week))]
      [else
       (print (reply-v2 user-response history))
       (doctor-driver-loop-v2 name (vector-append history (vector user-response)))])))

; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply user-response)
  (case (random 0 2) ; с равной вероятностью выбирается один из двух способов построения ответа
    ((0) (hedge-answer))  ; 1й способ
    ((1) (qualifier-answer user-response)))) ; 2й способ

(define (reply-v2 user-response history)
  ;; Рандомит в зависимости от того есть ли в истории хоть что-то
  ;; и есть ли в ответе ключевые слова
  (case (random (if (has-keywords? user-response) 0 1) (if (> (vector-length history) 0) 4 3))
    [(0) (suggestion-answer user-response)]
    ((1) (hedge-answer))
    ((2) (qualifier-answer user-response))
    ((3) (history-answer history))))

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

;; 3й способ -- выбор случайной фразы сказанной пациентом ранее
;; и упоменание, что он её говорил
(define (history-answer history)
  (append '(earlier you said that)
          (change-person (pick-random-vector history))))

;; 4й способ -- если было ключевые слова, то выбрать одно из них и составить
;; предложение по заранее подготовленному шаблону, при необходимости
;; использовав в нём одно из этих слово
(define (suggestion-answer user-response)
  '(suggestion))

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


(define keywords-structure
  '#( ; Формат : #(#(<ключевое_слово_1> <ключевое_слово_1> ...) #((<ответ_0>) (<ответ_1>) ...))
    #(#(depressed suicide exams university)
      #((when you feel depressed, go out for ice cream)
        (depression is a disease that can be treated)
        ()))

    #(#(mother father parents brother sister uncle aunt grandma grandpa)
      #((tell me more about your * , i want to know all about your *)
        (why do you feel that way about your * ?)
        (was your * kind to you?)
        (do you have any fond memories about * ?)
        (do you have any childhood memories about * ?)
        (did you always have this feelings towards * ?)))

    #(#(university scheme lections)
      	#((your education is important)
          (how much time do you spend on your studies ?)
          (education isnt easy but the result is worth it)
          (is your profession interesting to you)))

    #(#(husband wife children mother-in-law cheat cheating mother)
      #((your family needs you)
        (you may be annoyed by your family now, but your mind will change)
        (your close ones will change mind later)
        (maybe you should discuss this with your spouse)))))

;; Множество ведёр в хэш-множествах
(define buckets-cnt 256)
;; Номер ведра для заданного объекта
(define (bucket-num x)
  (remainder (equal-hash-code x) 256))
;; Добавляет в ведро новый элемент если он не содержался там ранее
;; возвращает потенциально обновлённый список
(define (add-to-bucket i bucket x)
  (if (member x bucket)
      bucket
      (cons x bucket)))

;; Множество ключевых слов необходимое для проверки наличия ключевого слова в строке
(define keywords-set
  (let*
      ;; Найдём вектор всех (возможно повторяющихся) ключевых слов
      ([all-keywords (vector-foldl
                      (λ (i accum el) (vector-append accum el))
                      #()
                      (vector-map (λ (x) (vector-ref x 0)) keywords-structure))])
    ;; Распределим их в "хэш-множество" из 256 ячеек внутри каждой из которых
    ;; хранится ведро в виде списка со всеми элементами имеющими хэш соответствующий
    ;; индексу
    ;; Строится вектор где для каждого n от 0 до buckets-cnt находятся все такие
    ;; элементы вектора all-keywords что их bucket-num соответствует номеру ведра.
    ;; После чего они сворачиваются в список с проверкой наличия в соответствующем
    ;; для каждого элемента
    ;; Если бы разрешены были бы мутаторы, то можно было бы наоборот, идти по списку и
    ;; пополнять вектор.
    (build-vector
     buckets-cnt
     (λ (bucket) (vector-foldl
                  add-to-bucket
                  '()
                  (vector-filter (λ (x) (= (bucket-num x) bucket)) all-keywords))))))

;; Проверить, содержит ли keywords-structure-keys заданный элемент
(define (keywords-set-member? x)
  (member x (vector-ref keywords-set (bucket-num x))))

;; Проверка содержатся ли в данном списке ключевые слова
(define (has-keywords? lst)
  (call/cc (λ (cc-exit)
             (for-each
              (λ (x) (when (keywords-set-member? x)
                       (cc-exit #t)))
              lst)
             #f)))

(define (filter-keywords lst)
  (filter keywords-set-member? lst))