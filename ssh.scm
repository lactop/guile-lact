; Набор специализированных процедур для работы с openssh

(define-module (lact ssh)
               #:use-module (srfi srfi-1)
               #:use-module (lact error-handling)
               #:export (ssh-command rsync-command shell-expression))

(define (string-inhabited? s) (and (string? s) (not (string-null? s))))
(define (strings-inhabited? . S) (every string-inhabited? S))

; (define (with-key . arguments)
;   (let ((k (fluid-ref ssh-key)))
;     (if (string-inhabited? k)
;         (cons* "-i" k arguments)
;         arguments)))
; 
; (define (ssh-command host words)
;   (cons "ssh"
;         (with-key
;           "-CTax" ; сжатие, без: терминала, ssh-агента и X11
;           "-o" "ConnectTimeout 1"
;           (format #f "~a@~a" (user) host)
;           (string-join (map (lambda (w) (format #f "'~a'" w)) words)))))

(define (clarify-user fu)
  (let ((eu (getenv "USER")))
    (if (string-inhabited? fu) 
        fu
        (if (string-inhabited? eu)
            (begin (dump-error "WARNING: Defaulting to ssh user ~a~%" eu)
                   eu)
            (begin (dump-error "WARNING: Defaulting to unspecified user~%")
                   "")))))

(define (user-at-host user)
  (let ((u (clarify-user user)))
    (if (string-inhabited? u)
        ; Если имя пользователя указано, то возвращаем приписыватель этого имени к
        ; адресу хоста
        (lambda (host) (format #f "~a@~a" u host))
        ; Иначе, возвращатель адреса хоста
        identity)))

; Надо формировать командные строки в виде списков слов. Нечто придётся
; пропускать, нечто -- добавлять, в зависимости от различных условий. Поэтому,
; стандартный трюк в стиле Клейсли с действиями pass и prepend.
(define pass identity)
(define (prepend . arguments) (lambda (l) (append arguments l))) 

(define (ssh-command user key command)
  (let ((part-1 (compose (prepend "ssh"
                                  "-CTax" ; сжатие, без: терминала, ssh-агента, X11
                                  "-o" "ConnectTimeout 1")
                         (if (string-inhabited? key) (prepend "-i" key) pass)))
        (at (user-at-host user))
        (command-part (if (string-inhabited? command) (prepend command) pass)))
    (lambda (host) ((compose part-1
                             (prepend (at host))
                             command-part)
                    '()))))

(define (shell-expression work-directory words)
  (cond ; Если нет списка слов команды, то делать нечего
        ((null? words) "")

        ; Если указана рабочая директория, то надо добавить к составленной из
        ; words команде переход в директорию.
        ((string-inhabited? work-directory) (string-append
                                              (format #f "cd '~a' && " work-directory)
                                              (shell-expression "" words)))

        (else (string-join (map (lambda (w) (format #f "'~a'" w)) words)))))

; Кажется, что подразумевать для синхронизации директории по умолчанию -- не
; удачная идея. Поэтому, ошибка, если директории не указаны.
(define (rsync-command user key source target)
  (unless (strings-inhabited? source target)
    (error "Source and target directories should be specified. Given:" source target))

  (let* ((rsync-path (lambda (t)
                       ; Несколько хитрое формирование строки-аргумента для
                       ; rsync-path.  После = командная строка должна быть без
                       ; группирующих кавычек.
                       (format #f "--rsync-path=mkdir -p '~a' && rsync" t)))

         (part-1 (compose (prepend "rsync" "-r")
                          (if (string-inhabited? key)
                              (prepend "-e" (format #f "ssh -i '~a'" key))
                              pass)
                          (prepend (rsync-path target) source)))
         (at (user-at-host user)))
    (lambda (host)
      (part-1 (list (format #f "~a:~a" (at host) target))))))
