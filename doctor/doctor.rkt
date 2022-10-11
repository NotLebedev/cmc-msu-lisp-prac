; заготовка "Доктора". Сентябрь 2022
#lang racket/base
(require racket/vector)

; В учебных целях используется базовая версия Scheme


;; ***** Реализация хэш-множества *****

;; Множество ведёр в хэш-множествах
(define buckets-cnt 256)
;; Номер ведра для заданного объекта
(define (bucket-num x)
  (remainder (equal-hash-code x) 256))

;; Хэш-множество из `buckets-cnt` ячеек внутри каждой из которых
;; хранится ведро в виде списка со всеми элементами имеющими хэш соответствующий
;; индексу
(define (build-set vctr)
  ;; Добавляет в ведро новый элемент если он не содержался там ранее
  ;; возвращает потенциально обновлённый список
  (define (add-to-bucket i bucket x)
    (if (member x bucket)
        bucket
        (cons x bucket)))
  ;; Строится вектор где для каждого n от 0 до buckets-cnt находятся все такие
  ;; элементы входжного вектора что их bucket-num соответствует номеру ведра.
  ;; После чего они сворачиваются в список с проверкой наличия в соответствующем
  ;; для каждого элемента
  ;; Если бы разрешены были бы мутаторы, то можно было бы наоборот, идти по списку и
  ;; пополнять вектор.
  (build-vector
   buckets-cnt
   (λ (bucket) (vector-foldl
                add-to-bucket
                '()
                (vector-filter (λ (x) (= (bucket-num x) bucket)) vctr)))))

;; Проверить, содержит ли множество `cset` заданный элемент
(define (set-member? cset x)
  (member x (vector-ref cset (bucket-num x))))


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

(define (reply-v1.5 user-response history)
  ;; Рандомит в зависимости от того есть ли в истории хоть что-то
  ;; и есть ли в ответе ключевые слова
  (case (random (if (has-keywords? user-response) 0 1) (if (> (vector-length history) 0) 4 3))
    [(0) (suggestion-answer user-response)]
    ((1) (hedge-answer))
    ((2) (qualifier-answer user-response))
    ((3) (history-answer history))))

;; Сконструировать вектор конфигураций вида
;; #(#(<предикат применимости> <вес стратегии> <функция, реализующая стратегию>) ... )
;; из списка агрументов, считая каждую тройку очередным элементом вектора
(define (build-config . lst)
  ;; Первые три элемента списка преобразуются в элемент конфигурации
  ;; В случае если это не (функция, число, функция) выбрасывает ошибку
  (define (head-to-entry lst)
    (if (and (procedure? (car lst)) (number? (cadr lst)) (procedure? (caddr lst)))
        (vector (car lst) (cadr lst) (caddr lst))
        (error 'wrong-init-types)))
  ;; Обработать вход по три элемента, первращая каждую тройку в новый элемент вектора
  (let loop ([rest lst] [res #()])
    (if (or (null? rest) (null? (cdr rest)) (null? (cddr rest)))
        res
        (loop (list-tail rest 3) (vector-append res (vector (head-to-entry rest)))))))

;; Получить из элемента списка конфигураций предикат применимости
(define (config-get-predicate config) (vector-ref config 0))
;; Получить из элемента списка конфигураций вес стратегии
(define (config-get-weight config) (vector-ref config 1))
;; Получить из элемента списка конфигураций функцию, реализующую стратегию
(define (config-get-strategy config) (vector-ref config 2))

(define reply-config
  ;; Список конфигураций стратегий. Каждый элемент имеет вид:
  ;; #(<предикат применимости> <вес стратегии> <функция, реализующая стратегию>)
  (build-config
   ;; Случайная заготовленная фраза
   (λ lst #t)
   1
   (λ lst (hedge-answer))

   ;; Замена лица + случайное начало
   (λ lst #t)
   1
   (λ (user-response . lst) (qualifier-answer user-response))

   ;; Если реплика содержит ключевые слова составить ответ по заготовленным шаблонам
   (λ (user-response . lst) (has-keywords? user-response))
   5
   (λ (user-response . lst) (suggestion-answer user-response))

   ;; Упомянуть ранее сказанное, заменив лицо в высказывание
   (λ (user-response history . lst) (> (vector-length history) 0))
   3
   (λ (user-response history . lst) (history-answer history))))

;; Вычислить массив стратегий применимых для текущего состояния "доктора"
(define (get-matching-strategies strategies user-response history)
  (vector-filter
   (λ (config) ((config-get-predicate config) user-response history))
   strategies))

;; Выбрать индекс из массива в соответствии с его весом
(define (pick-random-vector-with-weight weights)
  ;; Вычисляем сумарный вес и выбираем число в промежутке от 0 до веса
  (let* ([total (vector-foldl (λ (i total w) (+ total w)) 0 weights)] [rnd (random 0 total)])
    (call/cc
     (λ (cc-exit)
       ;; Считаем кумулятивные суммы по вектору весов. Как только кумулятивный
       ;; вес становится больше случайного возвращаем n-1 т.к. предыдущий отрезок
       ;; был w_{i-2} < rnd < w_{i-1}.
       (vector-foldl
        (λ (i cumsum w)
          (if (> cumsum rnd)
              (cc-exit (- i 1))
              (+ cumsum w)))
        0
        weights)
       ;; Если вышли из цикла, то rnd в точности равен максимуму.
       ;; Т.е. попали в последний отрезок
       (- (vector-length weights) 1)))))

(define (select-strategy strategies user-response history)
  (let* ([matching (get-matching-strategies strategies user-response history)]
         [weights (vector-map config-get-weight matching)])
    (vector-ref matching (pick-random-vector-with-weight weights))))

(define (reply-v2 user-response history)
  ((config-get-strategy (select-strategy reply-config user-response history)) user-response history))

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
  (vector-ref vctr (random 0 (vector-length vctr))))

;; случайный выбор одного из элементов непустого списка
(define (pick-random-list lst)
  (list-ref lst (random 0 (length lst))))

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
  ;; Выбираем случайное слово из списка ключевых слов в этом ответе.
  ;; Если какое-то слово встречается несколько раз, то в отфильтрованном
  ;; списке оно также присутствует несколько раз и выбирается с соответственной
  ;; вероятностью
  (keyword-get-answer (pick-random-list (filter-keywords user-response))))

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
   (λ (what) (replace-or-keep what replacement-pairs))
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

;; "Сырая" запись правил. Она обрабатывается при создании keywords-set и keywords-structre
;; чтобы поместить ключевые слова в хэш-множества для ускорения поиска и удаления дубликатов
(define keywords-raw
  '#( ; Формат : #(#(<ключевое_слово_1> <ключевое_слово_1> ...) #((<ответ_0>) (<ответ_1>) ...))
    #(#(depressed suicide exams university)
      #((when you feel depressed, go out for ice cream)
        (depression is a disease that can be treated)
        (hard parts of your life will always end well)
        (you should reasses your attitude to this problem)))

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
          (is your profession interesting to you ?)))

    #(#(husband wife children mother-in-law mother father)
      #((your family needs you)
        (you may be annoyed by your * now, but your mind will change)
        (your close ones will change mind later)
        (maybe you should discuss this with your *)))

    #(#(work boss burnout overwoking coworkers)
      #(you should try and resolve this problem with your manager)
      #(people often experience problems with * at their work)
      #(you should talk about * with your superior))))

;; Специальная форма keywords-raw где ключевые слова в каждой категории
;; хранятся в множестве, а не списке
(define keywords-structure
  (vector-map
   (λ (group)
     (let ([keywords (vector-ref group 0)] [answers (vector-ref group 1)])
       (vector (build-set keywords) answers)))
   keywords-raw))

;; Возвращает вектор ответов подходящих для данного ключевого слова.
;; Cперва находит все вектора ответов из подходящих групп, затем
;; объединяет их
(define (keyword-answers keyword)
  (vector-foldl
   (λ (i accum group) (vector-append accum (vector-ref group 1)))
   #()
   (vector-filter
    (λ (group)
      (let ([keywords (vector-ref group 0)])
        (set-member? keywords keyword)))
    keywords-structure)))

(define (keyword-get-answer keyword)
  (many-replace-v3
   (list (list '* keyword))
   (pick-random-vector (keyword-answers keyword))))

;; Множество ключевых слов необходимое для проверки наличия ключевого слова в строке
(define keywords-set
  (let*
      ;; Найдём вектор всех (возможно повторяющихся) ключевых слов
      ([all-keywords (vector-foldl
                      (λ (i accum el) (vector-append accum el))
                      #()
                      (vector-map (λ (x) (vector-ref x 0)) keywords-raw))])
    (build-set all-keywords)))

;; Проверить, содержит ли keywords-set заданный элемент
(define (keywords-set-member? x)
  (set-member? keywords-set x))

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
