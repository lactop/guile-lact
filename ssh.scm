; Набор специализированных процедур для работы с openssh

(define-module (lact ssh)
               #:use-module (lact error-handling)
               #:export (ssh-command with-ssh-credentials))

(define (string-inhabited? s) (and (string? s) (not (string-null? s))))

(define (clarify-user fu)
  (let ((eu (getenv "USER")))
    (if (string-inhabited? fu) 
        fu
        (if (string-inhabited? eu)
            (begin (dump-error "WARNING: Defaulting to ssh user ~a~%" eu)
                   eu)
            (begin (dump-error "WARNING: Defaulting to ssh user root~%")
                   "root")))))

; Надо формировать командные строки в виде списков слов. Нечто придётся
; пропускать, нечто -- добавлять, в зависимости от различных условий. Поэтому,
; трюк в стиле Клейсли с действиями pass и prepend.

(define pass identity)
(define (prepend . arguments) (lambda (l) (append arguments l)))

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
          "-o" "ConnectTimeout 1"
          (format #f "~a@~a" (user) host)
          (string-join (map (lambda (w) (format #f "'~a'" w)) words)))))

(define (ssh-command user key work-directory words))
