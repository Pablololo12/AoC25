(define-module (days day02)
  #:export (run)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 threads))

(define (getdig num)
  (cond
   ((< num 10) 1)
   ((< num 100) 2)
   ((< num 1000) 3)
   ((< num 10000) 4)
   ((< num 100000) 5)
   ((< num 1000000) 6)
   ((< num 10000000) 7)
   ((< num 100000000) 8)
   ((< num 1000000000) 9)
   ((< num 10000000000) 10)
   ((< num 100000000000) 11)
   (else (inexact->exact (+ 1 (floor (log10 num)))))))

(define (pow10 e)
  (letrec ((powrec (lambda (i acum)
                     (if (= i 0)
                         acum
                         (powrec (- i 1) (* acum 10))))))
    (powrec e 1)))

(define (legal num)
  (let* ((ndig (getdig num))
         (half (pow10 (quotient ndig 2))))
    (if (= (modulo ndig 2) 0)
        (if (= (modulo num half) (quotient num half))
              num
            0)
        0)))

(define (reccheck ac check group)
  (cond
   ((= ac 0) #t)
   ((= (modulo ac group) check) (reccheck (quotient ac group) check group))
   (else #f)))

(define (checkg rr num)
  (let ((group (pow10 rr)))
    (reccheck (quotient num group) (modulo num group) group)))

(define (legal2 num)
  (let* ((ndig (getdig num))
         (rr (iota (quotient ndig 2) 1)))
    (if (any (lambda (x) 
               (and (= 0 (modulo ndig x)) 
                    (checkg x num))) rr)
        num
        0)))

(define (count-range range func)
  (let ((rr (iota (+ 1 (- (cadr range) (car range))) (car range))))
    (fold (lambda (x acum)
            (+ acum (func x))) 0 rr)))

(define (algo input func)
  (fold (lambda (x acum)
          (+ acum (count-range x func))) 0 input))

(define (run input)
  (let* ((spli (string-split (car input) #\,))
         (newin 
          (map (lambda (x)
                 (map string->number (string-split x #\-))) spli)))
    (format #t "Part1: ~a~%Part2: ~a~%" (algo newin legal) (algo newin legal2))))
