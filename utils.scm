; Различные вспомогательные функции, не требующие взаимодействия с операционной
; системой

(define-module (lact utils)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-41)
               #:use-module ((ice-9 rdelim) #:select (read-line))
               #:use-module (ice-9 popen)
               #:export (head tail
                         port->string-stream pipe->string-stream
                         get-param
                         inhabited?
                         string-inhabited?
                         singleton?
                         true? false?
                         string-split-ne
                         dump dump-error
                         spawn
                         collect-pid
                         success? fail?
                         pass prepend))

; Различные доступы к элементам структур, состоящих из пар (списки)
(define head car)
(define tail cdr)

; Есть ли в контейнере элементы
(define string-inhabited? (compose not string-null?)) 
(define inhabited? (compose not null?)) 

; Список состоит из одного элемента?
(define (singleton? l) (and (pair? l) (null? (tail l))))  

(define (true? v) (and (boolean? v) v))
(define (false? v) (and (boolean? v) (not v)))

; Поток чтения строк из порта
(define-stream (port->string-stream p)
  (stream-let loop ((l (read-line p)))
    (if (eof-object? l)
      ; Если дочитали до конца, закрываем канал и завершаем поток
      (begin (close-input-port p) stream-null)
      ; В противном случае пытаемся продолжить поток с новой строкой
      (stream-cons l (loop (read-line p))))))

; Аналогично для каналов
(define-stream (pipe->string-stream p)
  (stream-let loop ((l (read-line p)))
    (if (eof-object? l)
        (begin (close-pipe p) stream-null)
        (stream-cons l (loop (read-line p))))))

; Получение параметра с номером n (счёт от 0) из командной строки cl. Если
; такого параметра нет, то list-ref выкидывает исключение, которое
; обрабатывается в обработчике, возвращающем значение default, которое и будет
; результатом выражения catch
(define (get-param cl n default)
  (catch 'out-of-range
         (lambda () (list-ref cl n))
         (lambda err default))) 

; Разбиение строки на элементы с выбором непустных элементов (non empty)
(define string-split-ne
  (compose (lambda (l) (filter string-inhabited? l)) string-split)) 

; Вывод всякого разного
(define (dumper p) (lambda (fmt . args)
                     (apply format p fmt args) (force-output p)))

(define dump (dumper (current-output-port)))
(define dump-error (dumper (current-error-port))) 

; Простой фоновый запуск команды
(define (spawn command)
  (let ((pid (false-if-exception (primitive-fork))))
    (if (zero? pid)
        (begin (false-if-exception (apply execlp (car command) command))
               (exit 1))
        pid)))

; Обёртка над waitpid: опрашивает данный pid, если процесс завершился возвращает
; pid и статус в паре, если нет, возвращает pid. Это удобно для collect-процедур
; в tracker-модулях
(define (collect-pid pid)
  (let ((r (waitpid pid WNOHANG))) (if (= pid (car r)) r pid)))

; Проверка статуса завершения процесса
(define success? (compose zero? status:exit-val))
(define fail? (compose not success?)) 

; Довольно часто приходится формировать списки по условиям: нечто нужно
; пропускать, нечто -- добавлять, в зависимости от различных условий. Поэтому,
; стандартный трюк в стиле Клейсли с действиями pass и prepend.

(define pass identity)
(define (prepend . arguments) (lambda (l) (append arguments l)))  
