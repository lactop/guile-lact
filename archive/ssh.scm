; Набор специализированных процедур для работы с openssh

(define-module (lact ssh)
               #:use-module (lact error-handling)
               #:export (ssh-command with-ssh-credentials))

; Передача учётных данных ssh через fluid: ограниченный своей явной областью
; видимости аналог глобальной переменной. Сделано в таком виде, чтобы другие
; процедуры, которые в итоге обращаются к ssh-command не заботились о явном
; протаскивании этих параметров через свои параметры. Пользователь и ключ в
; предполагаемых сценариях использования остаются постоянными, а узлы меняются.
; Постоянное передаётся через флюиды.

(define ssh-key (make-fluid ""))
(define ssh-user (make-fluid ""))

(define (string-inhabited? s) (and (string? s)
                                   (not (string-null? s))))

(define (user) (let ((fu (fluid-ref ssh-user))
                     (eu (getenv "USER")))
                 (if (string-inhabited? fu) 
                     fu
                     (if (string-inhabited? eu)
                         (begin (dump-error "WARNING: Defaulting to ssh user ~a~%" eu)
                                eu)
                         (begin (dump-error "WARNING: Defaulting to ssh user root~%")
                                "root")))))

(define (with-key . arguments)
  (let ((k (fluid-ref ssh-key)))
    (if (string-inhabited? k)
        (cons* "-i" k arguments)
        arguments)))

(define (with-ssh-credentials user key thunk)
  (with-fluids* (list ssh-user ssh-key) (list user key) thunk))

(define (ssh-command host words)
  (cons "ssh"
        (with-key
          "-CTax" ; сжатие, без: терминала, ssh-агента и X11
          "-o" "ConnectTimeout=1"
          (format #f "~a@~a" (user) host)
          (string-join (map (lambda (w) (format #f "'~a'" w)) words)))))
