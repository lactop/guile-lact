; Wed Mar 23 01:32:45 PM +05 2022
; 
; Видимо, это отдельная задача, на которую следует отдельно и смотреть. Сперва
; пощупаем то, что есть в DCPL. Это задача producer/consumer

(define node cons)
(define left car)
(define right cdr)
(define leaf? (compose not pair?)) 

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

(define (count-from-1 num)
  #f
  )

(define (add-first-1 length)
  #f
  )


; Thu Mar 24 11:09:49 PM +05 2022

; Так. Дальше обработка ошибок. Не сложно догадаться, что речь по
