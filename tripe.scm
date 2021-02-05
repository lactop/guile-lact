; Реализация запуска процесса на отдалённой машине через ssh и обеспечения с ним
; связи. Конструкция называется tripe, потому что triple pipe, потому что
; stdin, stdout, stderr

(define-module (lact tripe)
               #:use-module (ice-9 rdelim) 
               #:use-module (srfi srfi-9)
               #:use-module (srfi srfi-9 gnu)
               #:use-module (lact error-handling)
               #:use-module (lact ssh)
               #:export (open-ssh-tripe open-tripe close-tripe
                         tripe? tripe:input tripe:output tripe:error tripe:pid
                         read-all))

(define-immutable-record-type
  Tripe 
  (tripe in out err pid)
  tripe?
  (in tripe:input)
  (err tripe:error)
  (out tripe:output)
  (pid tripe:pid))

(define input car)
(define output cdr)

; Закрытие всех портов, за исключением перечисленных в списке ports. FIXME: Код
; наивный, если сравнивать с ice-9 popen. Необходимо лучше понимать Guile

(define (close-ports-excepting . ports)
  (port-for-each (lambda (p) (unless (member p ports) (close-port p)))))

(define (exec-with-pipes in out err words)
  ; (dump-error "executing: ~s~%" words)

  (close-ports-excepting in out err)

  (dup2 (port->fdes in) 0)
  (dup2 (port->fdes out) 1)
  (dup2 (port->fdes err) 2)

  (close-input-port in)
  (close-output-port out)
  (close-output-port err)
 
  (apply execlp (car words) words)

  (exit 1))

; words -- это слова командной строки в терминологии Bash

(define (open-tripe word . words)
  (let ((in (pipe))
        (out (pipe))
        (err (pipe)))
    (let ((p (primitive-fork)))
      (if (zero? p)
          (begin (close-output-port (output in))
                 (close-input-port (input out))
                 (close-input-port (input err))
                 (exec-with-pipes (input in)
                                  (output out)
                                  (output err)
                                  (cons word words)))
          (begin (close-input-port (input in))
                 (close-output-port (output out))
                 (close-output-port (output err))
                 (tripe (output in) (input out) (input err) p))))))

; (define (open-ssh-tripe host cmd . cmds)
;   (apply open-tripe (ssh-command host (cons cmd cmds))))

(define (close-tripe t)
  (close-output-port (tripe:input t))
  (close-input-port (tripe:output t))
  (close-input-port (tripe:error t))
  (kill (tripe:pid t) SIGTERM)
  (waitpid (tripe:pid t)))

(define (read-all p) (let ((v (read-line p)))
                       (when (not (eof-object? v))
                         (display v)
                         (newline)
                         (read-all p))))
