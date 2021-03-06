; Модуль с процедурами, необходимыми для обработки ошибок (в виде исключений)

(define-module (lact error-handling)
               #:use-module (srfi srfi-11)
               #:use-module (ice-9 format) 
               #:use-module (lact utils) 
               #:export (dump-parse-error
                         lact-error-handler
                         error-if-system-exception))

; Вывод сообщения об ошибке разбора. Форма данных об ошибке -- это список строк,
; первой из которых является исходная строка, за которым следует список строк с
; информацией об ошибках
(define (dump-parse-error err)
  (dump-error "Parsing: ~A~%" (head err))
  (for-each (lambda (str) (dump-error "~/~A~%" str)) (tail err)))

(define (dump-links requested given)
  (dump-error "Reading links failed; requested:~%")
  (let ((dump-item (lambda (s) (dump-error "~/~a~%" s))))
    (for-each dump-item requested)
    (dump-error "Given:~%")
    (for-each dump-item given)))

(define (check-type s) (lambda (v) (eq? v s)))
(define system-error? (check-type 'system-error))
(define bad-var-string? (check-type 'bad-var-string))
(define parse-error? (check-type 'parse-error))
(define readlink-failed? (check-type 'readlink-failed))

; Стандартный обработчик ошибок
(define (lact-error-handler clarification)
  (lambda (type . body)
    (cond ((system-error? type)
           (let-values (((fn fmt args code) (apply values body)))
             (if (string-null? clarification)
                 (apply dump-error (string-append fn ": " fmt "~%") args)
                 (apply dump-error (string-append clarification ": " fn ": " fmt "~%") args))))

          ((bad-var-string? type) (dump-error "Cannot parse var string: ~A~%" body))

          ((parse-error? type) (for-each dump-parse-error (head body))) 

          ((readlink-failed? type) (apply dump-links body))

          (else (apply throw type body)))
    ; Всегда возвращаем false, чтобы иметь возможность работать с ошибками во
    ; внешнем контексте
    #f))

; Видимо, довольно часто придётся возвращать системную ошибку, когда всё
; навернулось. Абстракция макросом
(define-syntax error-if-system-exception
  (syntax-rules ()
    ((error-if-system-exception expr ...)
     (catch 'system-error (lambda () expr ...) list)))) 
