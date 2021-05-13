(define-module (lact options)
               #:use-module (srfi srfi-1)
               #:use-module (srfi srfi-11)
               #:use-module (lact utils)
               #:export (options-usage options-flags
                         undash
                         gather-list gather-rest gather-string gather-boolean
                         default))

(define options-usage
  (make-fluid (lambda args (error "call to undefined usage:" args))))

(define options-flags
  (make-fluid (lambda args (error "call to undefined flags:" args))))

(define (flag? a) (and (= 2 (string-length a)) (eq? #\- (string-ref a 0))))

; (define (dash a) (string #\- a))

(define (undash a) (and (flag? a) (string-ref a 1)))

; with-unset проверяет, нужно ли проверить, что значение не установлено, и если
; нужно, проверяет, не установлено ли. Если по результатам проверки значение
; установлено, with-unset вызывает ошибку. Иначе, передаёт список в продолжение

(define (with-unset already-defined-message unset? v k)
  (if (or (string-null? already-defined-message)
          (unset? v))
      (k v)
      ((fluid-ref options-usage) already-defined-message v)))

(define (gather-list get set adm)
  (let ((usage (fluid-ref options-usage))
        (flags (fluid-ref options-flags)))
    (lambda (args opts)
      (with-unset adm null? (get opts)
                  (lambda (l)
                    (let-values (((words rest) (span (compose not flag?) (cdr args))))
                      (if (null? words)
                          (usage "no parameter list:" args)
                          (flags rest (set opts (fold cons l words))))))))))

(define (gather-rest get set adm)
  (let ((usage (fluid-ref options-usage)))
    (lambda (args opts)
      (with-unset adm null? (get opts)
                  (lambda (l)
                    (if (null? (cdr args))
                        (usage "no parameter list:" args)
                        ; gather-rest собирает все аргументы до конца командной
                        ; строки. До этого, по идее, для данных set/get аргумент
                        ; должен был собираться через gather-list, где всё
                        ; собиралось задом-наперёд. (cdr args) здесь добавляются
                        ; тоже задом-наперёд, а потом результат разворачивается
                        (set opts (reverse (fold cons l (cdr args))))))))))

(define (gather-string get set adm)
  (let ((usage (fluid-ref options-usage))
        (flags (fluid-ref options-flags)))
    (lambda (args opts)
      (with-unset adm string-null? (get opts)
                  (lambda (s)
                    (if (or (null? (cdr args)) (flag? (cadr args)))
                        (usage "no parameter:" args)
                        (flags (cddr args) (set opts (cadr args)))))))))

(define (gather-boolean get set already-message)
  (let ((flags (fluid-ref options-flags)))
    (lambda (args opts)
      (with-unset already-message false? (get opts)
                  (lambda (b)
                    (flags (cdr args) (set opts #t)))))))

(define (default get set unset? v)
  (lambda (o) (if (unset? (get o)) (set o v) o)))
