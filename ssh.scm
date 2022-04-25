; Набор специализированных процедур для работы с openssh

(define-module (lact ssh)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-9 gnu)
               #:use-module (srfi srfi-11)
               #:use-module (srfi srfi-41)
               #:use-module (ice-9 popen)
               #:use-module (ice-9 rdelim)
               #:use-module (lact utils)
               #:use-module (lact fs)
               #:use-module (lact error-handling)
               #:export (ssh-command rsync-command shell-expression
                         ssh-key-description
                         ensure-key! key:public key:private key:name))

(define (strings-inhabited? . S) (every string-inhabited? S))

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
        ; Если имя пользователя указано, то возвращаем приписыватель этого имени
        ; к адресу хоста
        (lambda (host) (format #f "~a@~a" u host))
        ; Иначе, возвращатель адреса хоста
        identity)))


(define (ssh-command user key command)
  (let ((part-1 (compose (prepend "ssh"
                                  ; сжатие, без: терминала, ssh-агента, X11 
                                  "-CTax" 
                                  "-o" "ConnectTimeout 1")
                         (if (string-inhabited? key) (prepend "-i" key) pass)))
        (at (user-at-host user))
        (command-part (if (string-inhabited? command) (prepend command) pass)))
    (lambda (host) ((compose part-1
                             (prepend (at host))
                             command-part)
                    '()))))

(define (shell-expression work-directory words)
  (cond
    ; Если нет списка слов команды, то делать нечего
    ((null? words) "")

    ; Если указана рабочая директория, то надо добавить к составленной из words
    ; команде переход в директорию.
    ((string-inhabited? work-directory)
     (string-append (format #f "cd '~a' && " work-directory)
                    (shell-expression "" words)))

    (else (string-join (map (lambda (w) (format #f "'~a'" w)) words)))))

; Кажется, что подразумевать для синхронизации директории по умолчанию -- не
; удачная идея. Поэтому, ошибка, если директории не указаны.
(define (rsync-command user key source target)
  (unless (strings-inhabited? source target)
    (error "Source and target directories should be specified. Given:"
           source
           target))

  (let* ((rsync-path (lambda (t)
                       ; Несколько хитрое формирование строки-аргумента для
                       ; rsync-path.  После = командная строка должна быть без
                       ; группирующих кавычек.
                       (format #f "--rsync-path=mkdir -p '~a' && rsync" t)))

         (part-1 (compose (prepend "rsync" "-rc" "--copy-links")
                          (if (string-inhabited? key)
                              (prepend "-e" (format #f "ssh -i '~a'" key))
                              pass)
                          (prepend (rsync-path target) source)))
         (at (user-at-host user)))
    (lambda (host)
      (part-1 (list (format #f "~a:~a" (at host) target))))))

; ПРОЦЕДУРЫ ДЛЯ РАБОТЫ С SSH-КЛЮЧАМИ

(define-immutable-record-type SSH-Key
  (ssh-key key pub path comment public private name)
  ssk-key?
  (key k:key)
  (pub k:pub)
  (path k:path)
  (comment k:comment)
  (public key:public)
  (private key:private)
  (name key:name)) 

(define ssh-key-description
  (let ((u (getenv "USER"))
        (h (gethostname)))
    (lambda (path service)
      (let* ((n (format #f "~a-~a-~a-ecdsa-key"
                        (if (string-null? service) "generic" service)
                        u
                        h))
             (C (format #f "~a key for ~a@~a" n u h))
             (P (format #f "~a/~a" path n))
             (path-items (split-path path)))
        (ssh-key P (string-append P ".pub") path-items C "" "" n)))))

(define (key-exists? k)
  ; В content список из пар (имя . содержимое файла по мнению утилиты file)
  (let* ((key (k:key k))
         (pub (k:pub k))
         (content (stream->list
                    (stream-map
                      cons
                      (stream key pub)
                      (pipe->string-stream
                        (open-pipe* OPEN_READ "file" "-LNb" key pub)))))
         (key-content (assoc key content))
         (pub-content (assoc pub content)))
    (and (pair? key-content)
         (pair? pub-content)
         (string=? "OpenSSH private key" (cdr key-content))
         (string=? "OpenSSH ECDSA public key" (cdr pub-content)))))

(define (ssh-keygen k)
  (let ((path (k:key k))
	(comment (k:comment k)))
    (when (fail? (system* "ssh-keygen" "-tecdsa" "-b384"
                          "-N" ""
                          "-C" comment
                          "-f" path))
      (error (format #f "~a generation failed: ~a" comment path)))))

(define (ensure-key! k)
  (if (key-exists? k)
      (dump-error "key exists: ~a~%" (k:key k))
      (begin (ensure-path! (k:path k))
             (ssh-keygen k)))
  ; Пара строк, содержащих открытый и закрытый ключи
  (set-fields k
              ((key:public) (with-input-from-file (k:pub k) read-line))
              ((key:private) (string-join (stream->list
                                            (port->string-stream (open-input-file (k:key k))))
                                          (string #\newline)))))
