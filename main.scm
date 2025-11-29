(use-modules (downloader)
             (ice-9 format))

(define (main args)
  (cond
   ((null? (cdr args))
    (display "Running everything!\n"))
   (else
    (let* ((day-num (string->number (cadr args)))
           (day-mod (format #f "day~2,'0d" day-num))
           (mod-name `(days ,(string->symbol day-mod)))
           (mod (resolve-interface mod-name))
           (run-func (module-ref mod 'run))
           (input (get-day day-num)))
      (format #t "Running day~2,'0d~%" day-num)
      (run-func input)))))

(main (command-line))
