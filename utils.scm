; Различные вспомогательные функции, не требующие взаимодействия с операционной
; системой

(define-module (lact utils)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-41)
               #:use-module ((ice-9 rdelim) #:select (read-line))
               #:use-module (lact error-handling)
               #:export (head tail port->string-stream get-param))

; Различные доступы к элементам структур, состоящих из пар (списки)
(define head car)
(define tail cdr)

; Поток чтения строк с переменными
(define-stream (port->string-stream p)
  (stream-let loop ((l (read-line p)))
    (if (eof-object? l)
      ; Если дочитали до конца, закрываем канал и завершаем поток
      (begin (close-port p) stream-null)
      ; В противном случае пытаемся продолжить поток с новой строкой
      (stream-cons l (loop (read-line p))))))

; Получение параметра с номером n (счёт от 0) из командной строки cl. Если
; такого параметра нет, то list-ref выкидывает исключение, которое
; обрабатывается в обработчике, возвращающем значение default, которое и будет
; результатом выражения catch
(define (get-param cl n default)
  (catch 'out-of-range
         (lambda () (list-ref cl n))
         (lambda err default))) 
