(define (flag? a) (and (= 2 (string-length a)) (eq? #\- (string-ref a 0))))

(define (dash a) (string #\- a))
(define (undash a) (and (flag? a) (string-ref a 1)))

; with-unset проверяет, нужно ли проверить, что значение не установлено, и если
; нужно, проверяет, не установлено ли. Если по результатам проверки значение
; установлено, with-unset вызывает ошибку. Иначе, передаёт список в продолжение.

(define (with-unset am unset? v k)
  (if (or (string-null? am)
          (unset? v))
      (k v)
      (error am v)))

(define (gather-list flag get set already-message)
  (lambda (args opts)
    (with-unset already-message null? (get opts)
                (lambda (l)
                  (let-values (((words rest) (span (compose not flag?) args)))
                    (if (null? words)
                        (error "no parameter list for:" (dash flag))
                        (flags rest (set opts (fold cons l words)))))))))

(define (gather-rest flag get set already-message)
  (lambda (args opts)
    (with-unset already-message null? (get opts)
                (lambda (l)
                  (if (null? args)
                      (error "no parameter list for:" (dash flag))
                      (set opts (reverse (fold cons l args))))))))

(define (gather-string flag get set already-message)
  (lambda (args opts)
    (with-unset already-message string-null? (get opts)
                (lambda (s)
                  (if (null? args)
                      (error "no parameter for:" (dash flag))
                      (flags (cdr args) (set opts (car args))))))))

(define (gather-boolean flag get set already-message)
  (lambda (args opts)
    (with-unset already-message (lambda (v) (and (boolean? v) (not v))) (get opts)
                (lambda (b)
                  (flags args (set opts #t))))))

(define (default get set unset? v) (lambda (o) (if (unset? (get o))
                                                   (set o v)
                                                   o)))
