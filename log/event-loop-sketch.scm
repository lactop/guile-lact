; Wed Mar 23 01:32:45 PM +05 2022
; 
; Видимо, это отдельная задача, на которую следует отдельно и смотреть. Сперва
; пощупаем то, что есть в DCPL. Это задача producer/consumer

(define pair cons)
(define fst car)
(define snd cdr)

(define node cons)
(define left fst)
(define right snd)
(define leaf? (compose not pair?)) 

(define dump
  (let ((p (current-output-port)))
    (lambda (fmt . args)
      (apply format p (string-append fmt "~%") args))))

; Двойной проход. Это не годится, потому что проход по дереву выполняется
; последовательно и целиком. Не подойдёт для обработки потока событий, по
; которому нельзя пройти целиком за раз.

(define (height*sum-1 tree)
  (letrec ((height (lambda (t)
                     (if (leaf? t) 0 (+ 1 (max (height (left t))
                                               (height (right t)))))))
           (sum (lambda (t)
                  (if (leaf? t) t (+ (sum (left t)) (sum (right t)))))))
    (* (height tree) (sum tree))))

; Совместный проход. Он будет один, но основная процедура - inner в данном
; случае - сформирована заранее. А нам нужна динамика: открываются/закрвыаются
; сокеты, приходят запросы на проведение тестов. Появляются сообщения для
; telegram.

(define (height*sum-2 tree)
  (letrec ((inner (lambda (t) (if (leaf? t)
                                (cons 0 t)
                                (let ((left-height&sum (inner (left t)))
                                      (right-height&sum (inner (right t))))
                                  (cons (+ 1 (max (car left-height&sum) (car
                                                                          right-height&sum)))
                                        (+ (cdr left-height&sum) (cdr right-height&sum))))))))
    (let ((height&sum (inner tree)))
      (* (car height&sum) (cdr height&sum)))))

; Варианта с продолжениями. Но пока здесь нет никакого взаимодействия. Структура
; фиксирована

(define (height*sum-3 tree)
  ((rec inner (lambda (t receiver)
                (if (leaf? t)
                  (receiver 0 t)
                  (inner (left t)
                         (lambda (h1 s1)
                           (inner (right t)
                                  (lambda (h2 s2) (receiver (+ 1 (max h1 h2))
                                                            (+ s1 s2)))))))))
   tree *))

; Нелокальные выходы. Эта тема уже, кажется, более близкой. Потому что каждая
; запрошенная операция должна выводить куда-то в select-цикл.

; Хорошо, снова модельная процедурка

(define (tree-product-1 tree)
  (if (leaf? tree) tree (* (tree-product (left tree))
                           (tree-product (right tree)))))

(tree-product-1 (node (node 2 3) (node 4 5)))
; => 120

(tree-product-1 (node (node 0 4) (node 4 5)))
; => 0

; Версия с продолжениями. Пока без нелокальных выходов. Всё внутри

(define (tree-product-2 tree k)
  (if (leaf? tree)
    (k tree)
    (tree-product-2 (left tree)
                    (lambda (vl)
                      (tree-product-2 (right tree)
                                      (lambda (vr) (k (* vl vr))))))))

(tree-product-2 (node (node 2 3) (node 4 5)) identity)
; => 120

(tree-product-2 (node (node 0 4) (node 4 5)) identity)
; => 0

; Версия с нелокальным выходом (?)... В k-inner, получается 

(define (tree-product-3 tree k-outer)
  ((rec prod
        (lambda (t k-inner)
          (if (leaf? t)
            (if (= t 0) (k-outer 0) (k-inner t))
            (prod (left t)
                  (lambda (vl) (prod (right t)
                                     (lambda (vr) (k-inner (* vl vr)))))))))
   tree k-outer))

(tree-product-3 (node (node 2 3) (node 4 5)) identity)
; => 120

(tree-product-3 (node (node 0 4) (node 4 5)) identity)
; => 0

; Далее нам предлагается немного это всё дело оптимизировать. По коду видно, что
; рекурсия хвостовая. То есть, когда встречается 0, происходит сразу возврат из
; tree-product-4

(define (tree-product-4 tree)
  ((rec prod (lambda (t k)
               (if (leaf? t)
                 (if (= t 0)
                   0
                   (k t))
                 (prod (left t)
                       (lambda (vl) (prod (right t)
                                          (lambda (vr) (k (* vl vr)))))))))
   tree identity))

(tree-product-4 (node (node 2 3) (node 4 5)))
; => 120

(tree-product-4 (node (node 0 4) (node 4 5)))
; => 0

; Дальше coroutines. Подбираемся ближе к делу, кажется. Нам обещают независимые
; процессы, которые могут обмениваться данными. Producer/consumer. В роли
; producer может быть select-цикл.

(define (count-from num)
  ((rec new-producer (lambda (n)
                       (format #t "generated: ~a~%" n)
                       (lambda (consumer) (consumer n (new-producer (+ n 1))))))
   num))

(define (add-first length)
  ((rec new-consumer (lambda (len sum)
                       (lambda (value next-producer)
                         (format #t "consumed: ~a~%" value)
                         (if (= len 0)
                           sum
                           (next-producer (new-consumer (- len 1) (+ value sum)))))))
   length 0))

; Логика выше весьма хитрая. Тут тоже везде хвостовая рекурсия и это
; перепутывание двух процессов вполне эффективно. Стек не съедает, по крайней
; мере. Для понимания важно этот пример интерпретировать на качественном уровне.
; Я смотрю на это так: lambda формирует паузу в вычислении. Она говорит: я не
; знаю, что мне дальше делать, дай мне информацию. Можно на это смотреть, как на
; точки останова.
;
; Важно отметить, что информация об этих точка куда-то возвращается и там дальше
; используется для возвращения в вычисление. 

((count-from 3) (add-first 5))
((count-from 3) (add-first 1))

; Однако, реализация не особо корректная, потому что выполняются лишние
; действия, которые не были запрошены. Не аккуратно:

((count-from 3) (add-first 0))
; generated: 3
; generated: 4
; consumed: 3
; $1 = 0

; В книге предлагается поправить это всё. Надо сделать для лучшего понимания.

; Fri Mar 25 08:39:21 AM +05 2022

(define (count-from-1 num)
  ((rec produce (lambda (n)
                  (lambda (consumer)
                    ; Нечто новое формируется, когда есть consumer
                    (format #t "generating: ~a~%" n)
                    (consumer n
                              ; Решать нужна ли следующая итерация будет consumer
                              (lambda () (produce (1+ n)))))))
   num))

(define (add-first-1 length)
  ((rec consume (lambda (len sum)
                  (lambda (value producer)
                    (if (zero? len)
                      sum
                      (begin
                        (format #t "consuming: ~a~%" value)
                        ((producer) (consume (1- len) (+ sum value))))))))
   length 0))

; Этот вариант лучше
((count-from-1 3) (add-first-1 1))
; generating: 3
; consuming: 3
; generating: 4
; => 3

; Но, всё равно, одно значение генерируется, даже если оно не нужно
((count-from-1 20) (add-first-1 0))
; generating: 20
; =>  0

; Хорошо. Вопрос: а как генерировать исключительно по запросу? В генераторе
; только одна переменная состояния, дополнительный rec не нужен



(define (count-from-2 num)
  (dump "P: expecting consumer")
  (lambda (consumer)
    (dump "P: consumer ~s" consumer)
    (consumer
      (lambda (next)
        (format #t "P: next: ~a~%" next)
        (format #t "P: generating: ~a~%" num)
        (next num (count-from-2 (1+ num)))))))

(define (add-first-2 length)
  ((rec consume
        (lambda (len sum)
          (dump "C: expecting producer")
          (lambda (producer)
            (dump "C: producer ~s" producer)
            (if (zero? len)
              sum
              (producer (lambda (v next)
                          (format #t "C: consuming: ~a, next: ~s~%" v
                                  next)
                          (next (consume (1- len) (+ v sum))))))))) ; †
   length 0))

((count-from-2 3) (add-first-2 2)) ; ‡

; Видно по отладочному коду, насколько это всё не очевидно. Проблема была в
; строке †, которая должна повторять вызов верхнего уровня в строке ‡. Чистые
; варианты:

(define (count-from-2 num)
  (lambda (consumer)
    (consumer
      (lambda (next)
        (dump "P: generating: ~a" num)
        (next num (count-from-2 (1+ num)))))))

(define (add-first-2 length)
  ((rec consume
        (lambda (len sum)
          (lambda (producer)
            (if (zero? len)
              sum
              (producer (lambda (v next)
                          (dump "C: consuming: ~s" v)
                          (next (consume (1- len) (+ v sum)))))))))
   length 0))

((count-from-2 3) (add-first-2 2))
; P: generating: 3
; C: consuming: 3
; P: generating: 4
; C: consuming: 4
; => 7

((count-from-2 4) (add-first-2 0))
; => 0

; Fri Mar 25 01:30:30 PM +05 2022

; Дальше обработка ошибок. Видимо, про failure- и success-продолжения. На работы
; с окружениями

(define (env-empty) (list))

(define (env-lookup name env succ fail)
  (if (null? env)
    (fail)
    (let ((binding (car env)))
      (if (eq? name (fst binding))
        (succ (snd binding))
        (env-lookup name (cdr env) succ fail)))))

(define (env-extend-loop name value env succ fail)
  (if (null? env)
    (fail)
    (let ((binding (car env)))
      (if (eq? name (fst binding))
        (succ (cons (pair name value) (cdr env)))
        (env-extend-loop name value (cdr env)
                         (lambda (bindings) (succ (cons binding bindings)))
                         fail)))))

; В env-extend-loop довольно хитрая рекурсия. Она хвостовая, но в
; succ-передаётся всё более сложная lambda. Однако lambda-ы в Scheme
; относительно дешёвые. Это просто пара ссылок на окружение и на код. Поэтому
; получается нечто вроде связного списка, где связь устанавливается через
; предыдущую lambda, а именно, через succ. Полная картина такая: ссылка на succ
; живёт в окружении, на это окружение будет ссылка из новой lambda, которая
; станет следующим succ-продолжением. Абстрактно, это выглядит, как связный
; список. Конкретно, конечно, не самая эффективная его представление.

(define (env-extend name value env)
  (env-extend-loop name value env
                   (lambda (bindings) bindings)
                   (lambda () (cons (pair name value) env))))

(define (env-merge env1 env2)
  (fold-right (lambda (binding env) (env-extend (fst binding) (snd binding) env))
              env2
              env1))

(define E1 (env-extend 'a 3 (env-extend 'b 2 (env-extend 'a 1 (env-empty)))))

(define (env-test1 names env)
  (map (lambda (name) (env-lookup name env identity (const '*unbound*))) names))

(env-test1 '(a b c) E1)
; => (3 2 *unbound*)

(define (env-test2 names env)
  (let loop ((ns names)
             (k identity))
    (if (null? ns)
      (k '())
      (env-lookup (car ns)
                  env
                  (lambda (val) ; †
                    (loop (cdr ns) (lambda (bindings)
                                     (k (cons (pair (car ns) val) bindings)))))
                  (lambda () (loop (cdr ns) k)))))) ; ‡

(env-test2 '(a b c) E1)
; => ((a . 3) (b . 2))

; Работает env-test2 по встретившемуся уже принципу: замыкания использованы в
; виде связного списка. Связь устанавливается через ссылку в окружении на k и
; через ссылку на некоторый подсписок имён. Остальные идентификаторы в
; продолжениях † и ‡ привязаны к окружению верхнего уровня, или связаны
; lambda-ой. Не самый эффективный способ, но и не самый неэффективный.

; Можно применить эту же идею к конструкции самих окружений

(define (env-empty) (lambda (name succ fail) (fail)))

(define (env-lookup name env succ fail) (env name succ fail))

(define (env-extend name value env) (lambda (name-needed succ fail)
                                      (if (eq? name name-needed)
                                        (succ value)
                                        (env name-needed succ fail))))

(define (env-merge e1 e2)
  (lambda (name succ fail)
    (e1 name succ (lambda () (e2 name succ fail)))))

; Здесь переопределена конструкция окружений. Но они работают по-прежнему.
(define E1 (env-extend 'a 1 (env-extend 'b 2 (env-extend 'c 3 (env-empty)))))
(env-test2 '(a c b d e f) E1)
; >= ((a . 1) (c . 3) (b . 2))

; Sat Mar 26 10:28:33 AM +05 2022

; Дальше по списку перебор с возвратами (aka backtracking). На примере задачи
; SAT. Это поиск значений переменных, удовлетворяющих логической формуле.

(define (structure? op arity) (lambda (form) (and (list? form)
                                                (= (1+ arity) (length form))
                                                (eq? op (car form)))))
(define not? (structure? 'not 1))
(define negand second) ; Это аргумент отрицания 

(define and? (structure? 'and 2))
(define conjunct-1 second)
(define conjunct-2 third)

(define or? (structure? 'or 2))
(define disjunct-1 second)
(define disjunct-2 third)

(define bf1 '(and (or a (or b c)) (and (not a) (not b))))

(define (satisfy formula)
  (sat formula
       (env-empty)
       (lambda (b asst fail) (if b asst (fail)))
       (lambda () 'failed)))

; Традиционно: φ - формула, Γ - окружение, назначение переменных
(define (sat φ Γ succ fail)
  (cond ((boolean? φ) (succ φ Γ fail))
        ((symbol? φ)
         (env-lookup φ Γ
                     (lambda (b) (succ b Γ fail))
                     (lambda ()
                       (succ #t
                             (env-extend φ #t Γ)
                             (lambda ()
                               (succ #f (env-extend φ #f Γ) fail))))))
        ((not? φ)
         (sat (negand φ) Γ 
              (lambda (b Γ1 fail1) (succ (not b) Γ1 fail1))
              fail))
        ((and? φ)
         (sat (conjunct-1 φ) Γ
              (lambda (b1 Γ1 fail1)
                (if b1
                  (sat (conjunct-2 φ) Γ1 succ fail1)
                  (succ #f Γ1 fail1)))
              fail))
        ((or? φ)
         (sat (disjunct-1 φ) Γ
              (lambda (b1 Γ1 fail1)
                (if b1
                  (succ #t Γ1 fail1)
                  (sat (disjunct-2 φ) Γ1 succ fail1)))
              fail))
        (else (error 'illegal-form φ))))

(satisfy bf1)
; => ((c . #t) (b . #f) (a . #f))

(satisfy '(and (not a) a))
; =>  failed

; Хорошо, вроде как, работает. Это довольно хитрый цикл со множеством курсоров,
; которые показывают на текущие участки формул и на откаты через fail. Вопрос,
; конечно, насколько это эффективно и насколько это неэффективно.

; Чтобы построить структуру перебора по структуре формулы и чтобы явно не
; управлять всем этим протаскиванием (threading) окружений и откатов, которые
; однообразны, можно использовать каррирование. Мы выносим точки отката и
; окружения

(define (satisfy φ)
  ((sat φ (lambda (b) (lambda (fail Γ) (if b Γ (fail)))))
   (lambda () 'failed)
   (env-empty)))

(define (sat φ succ)
  (cond ((boolean? φ) (succ φ))
        ((symbol? φ)
         (lambda (fail Γ)
           (env-lookup φ Γ
                       (lambda (b) ((succ b) fail Γ))
                       (lambda () ((succ #t)
                                   (lambda () ((succ #f) fail (env-extend φ #f Γ)))
                                   (env-extend φ #t Γ))))))
        ((not? φ) (sat (negand φ) (lambda (b) (succ (not b)))))
        ((and? φ) (sat (conjunct-1 φ)
                       (lambda (b1) (if b1 (sat (conjunct-2 φ) succ) (succ #f)))))
        ((or? φ) (sat (disjunct-1 φ)
                      (lambda (b1) (if b1 (succ #t) (sat (disjunct-2 φ) succ)))))
        (else (error 'illegal-form φ))))

(satisfy bf1)
(satisfy '(or a b))
