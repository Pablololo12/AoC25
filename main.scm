(use-modules (downloader)
             (ice-9 format))

(define (run-day day)
  (let* ((day-mod (format #f "day~2,'0d" day))
         (mod-name `(days ,(string->symbol day-mod)))
         (mod (resolve-interface mod-name))
         (run-func (module-ref mod 'run))
         (input (get-day day)))
    (format #t "Running ~a~%" day-mod)
    (run-func input)))

(define (main args)
  (cond
   ((null? (cdr args))
    (let loop ((days '(1 2 3 4)))
      (unless (null? days)
        (run-day (car days))
        (loop (cdr days)))))
   (else
    (let ((day-num (string->number (cadr args))))
      (run-day day-num)))))

(main (command-line))
