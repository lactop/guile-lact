(define-module (lact loop)
               #:use-module (lact legacy utils)
               #:export (timeout terminate select-loop))

(define (timeout t)
  (if (not (number? t))
    (terminate "timeout is not a number: ~s~%" t)
    (call/cc (lambda (k) (list 'timeout t k)))))

(define (terminate . message)
  (unless (null? message)
    (catch
      'misc-error
      (lambda () (apply dump-error message))
      ; tag = misc-error, f1 и f2 некоторые значения, которые в моих
      ; экспериментах всегда #f (ложь)
      (lambda (tag subroutine fmt args rest) 
        (dump-error "reporting of ~s failed with reason: ~a~%"
                    message
                    (apply format #f fmt args)))))
  '(terminate))
